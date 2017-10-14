Writing a Concurrency Testing Library (Part 1)
==============================================

Welcome to the first part of a tutorial on writing your very own
concurrency testing library for Haskell.  Before we get into the
details, let's just clarify what I mean by a "concurrency testing
library".  The goal is a function which, given some concurrent Haskell
like so:

```haskell
example = do
  a <- newEmptyMVar
  forkIO (putMVar a 1)
  forkIO (putMVar a 2)
  takeMVar a
```

Will tell us the possible results of that computation:

```
λ> test example
[1, 2]
```

We're going to build this from the ground up, using
the [concurrency][] library, as it provides a typeclass abstraction
over forking, MVars, STM, and suchlike.

[concurrency]: https://hackage.haskell.org/package/concurrency

You may have come across my [dejafu][] library before.  If not, don't
worry, but you may want to check it out as we're going to be building
something very similar.

[dejafu]: https://hackage.haskell.org/package/dejafu


Let's get down to business
--------------------------

Ok, with the preliminaries over, let's get coding!  All the code
written in this series is on [GitHub][], with one tag for each post.
The code for this post is under the "post-01" tag.

[GitHub]: https://github.com/barrucadu/minifu

The goal in this post is to be able to implement a function which can
execute simple thread-and-MVar computations (like the example from the
beginning) with a stateful scheduler.  Firstly, let's say what we
know:

- We're using the `MonadConc` typeclass from [concurrency][], rather
  than `IO`.
- We want to be able to examine arbitrary `MonadConc` computations.
- We also want to be able to pause and resume "threads" at will, so we
  can explore different executions.

That sounds rather like something based on continuations or a free
monad.  Furthermore, we're going to need mutable state to implement
all of this, as we're modelling a DSL with mutable references, and
doing that purely is a huge pain.

Let's write down some types.  Because we're writing a mini-dejafu, I'm
calling this project "minifu".  So we want a function:

```haskell
import qualified Control.Concurrent.Classy as C
import Data.List.NonEmpty (NonEmpty(..))

newtype ThreadId = ThreadId Int
  deriving (Eq, Ord)

type Scheduler s = NonEmpty ThreadId -> s -> (ThreadId, s)

minifu :: C.MonadConc m => Scheduler s -> s -> MiniFu m a -> m (Maybe a, s)
```

For some suitable `MiniFu` monad transformer.  Now we're going to take
the standard way of constructing a free monad, and have a data
structure representing our class of interest (`MonadConc`), with one
constructor for every function.  Because we're only talking about
threads and MVars in this post, it will be a fairly small type:

```haskell
{-# LANGUAGE GADTs #-}

data PrimOp m where
  Fork         :: MiniFu m () -> (ThreadId -> PrimOp m) -> PrimOp m
  NewEmptyMVar :: (MVar m a -> PrimOp m)                -> PrimOp m
  PutMVar      :: MVar m a -> a -> PrimOp m             -> PrimOp m
  TakeMVar     :: MVar m a -> (a -> PrimOp m)           -> PrimOp m
  Stop         :: m ()                                  -> PrimOp m

newtype MVarId = MVarId Int
  deriving (Eq, Ord)

data MVar m a = MVar
  { mvarId  :: MVarId
  , mvarRef :: C.CRef m (Maybe a)
  }
```

The `Stop` action is what is going to let us communicate the final
result out of the computation.  I've also defined an `MVar` type.  Our
MVars are going to be implemented as a `CRef` (what [concurrency][]
calls an `IORef`) holding a maybe value, along with an identifier.
These identifiers will come into play when we look at threads
blocking.

Given this set up, the `MiniFu` type is very simple:

```haskell
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import qualified Control.Monad.Cont as K

newtype MiniFu m a = MiniFu { runMiniFu :: K.Cont (PrimOp m) a }
  deriving (Functor, Applicative, Monad)
```

We're not actually going to write a `MonadConc` instance for `MiniFu`
yet, because there are a bunch of constraints which we can't really
satisfy.  But we can still define the functions of interest:

```haskell
fork :: MiniFu m () -> MiniFu m ThreadId
fork ma = MiniFu (K.cont (Fork ma))

newEmptyMVar :: MiniFu m (MVar m a)
newEmptyMVar = MiniFu (K.cont NewEmptyMVar)

putMVar :: MVar m a -> a -> MiniFu m ()
putMVar v a = MiniFu (K.cont (\k -> PutMVar v a (k ())))

takeMVar :: MVar m a -> MiniFu m a
takeMVar v = MiniFu (K.cont (TakeMVar v))
```

Hey, not bad!  Now we can slap a `MiniFu m Int` type signature on our
example from the start (and rename the `forkIO` calls) and it
compiles!

```haskell
example :: MiniFu m Int
example = do
  a <- newEmptyMVar
  fork (putMVar a 1)
  fork (putMVar a 2)
  takeMVar a
```

Take a moment to make sure you're happy with this section before
moving on to the next.  MiniFu is going to be a layered application:
this is the basic layer which defines the functions we can test; the
next layer executes a MiniFu computation; the layers above that will
implement the systematic testing behaviour.


Implementing `minifu`
---------------------

Recall the type of `minifu`:

```haskell
minifu :: C.MonadConc m => Scheduler s -> s -> MiniFu m a -> m (Maybe a, s)
```

So, what does it need to do?  It needs to set up the execution
environment: in this case that's specifying that the provided
computation is the main thread, and then it needs to repeatedly call
the scheduler, executing one `PrimOp` of the chosen thread at a time,
until either the main thread terminates or everything is blocked.

In the best functional programming practice, `minifu` is going to do
the minimum it can and call another function to do the rest.  So what
`minifu` is *actually* going to do is to extract the continuation and
set up the mechanism to communicate the final result back:

```haskell
minifu sched s (MiniFu ma) = do
  out <- C.newCRef Nothing
  s'  <- run sched s (K.runCont ma (Stop . C.writeCRef out . Just))
  a   <- C.readCRef out
  pure (a, s')
```

Before we move on to the implementation of `run`, let's first look at
two concerns we'll have along the way: getting unique names (for
threads and MVars) and representing threads.

### Names

Each thread gets a unique `ThreadId`, and each MVar gets a unique
`MVarId`.  As these are just an `Int`, we can use the same source for
both:

```haskell
type IdSource = Int

initialIdSource :: IdSource
initialIdSource = 0

nextThreadId :: IdSource -> (ThreadId, IdSource)
nextThreadId n = (ThreadId n, n + 1)

nextMVarId :: IdSource -> (MVarId, IdSource)
nextMVarId n = (MVarId n, n + 1)
```

This is as simple as it gets, but it's good enough for now.

### Threads

What is a thread?  Well, it has a continuation, which is some value of
type `PrimOp m`, and it might be blocked.  We want to know if a thread
is blocked for two reasons: we don't want the scheduler to schedule a
blocked thread, and we want to be able to tell if the computation is
deadlocked.  Threads can only block on reading from or writing to
MVars (currently), so let's use a `Maybe MVarId` to indicate whether
the thread is blocked:

```haskell
data Thread m = Thread
  { threadK     :: PrimOp m
  , threadBlock :: Maybe MVarId
  }
```

When we create a thread, it's initially unblocked:

```haskell
thread :: PrimOp m -> Thread m
thread k = Thread
  { threadK     = k
  , threadBlock = Nothing
  }
```

And finally we need a way to construct our initial collection of
threads:

```haskell
import Data.Map (Map)
import qualified Data.Map as M

type Threads m = Map ThreadId (Thread m)

initialise :: PrimOp m -> (Threads m, IdSource)
initialise k =
  let (tid, idsrc) = nextThreadId initialIdSource
  in (M.singleton tid (thread k), idsrc)
```

And now back to the implementation of `minifu`.

### Implementing `run`

The `run` function is responsible for taking the first continuation,
creating the collection of threads, and repeatedly calling the
scheduler and stepping the chosen thread, until the computation is
done.

It has this type:

```haskell
run :: C.MonadConc m => Scheduler s -> s -> PrimOp m -> m s
```

As with `minifu`, we shall keep it simple, and delegate most of the
work to yet another function:

```haskell
import Data.List.NonEmpty (nonEmpty)
import Data.Maybe (isNothing)

run sched s0 = go s0 . initialise where
  go s (threads, ids)
    | initialThreadId `M.member` threads = case runnable threads of
      Just tids ->
        let (chosen, s') = sched tids s
        in go s' =<< stepThread chosen (threads, ids)
      Nothing -> pure s
    | otherwise = pure s

  runnable = nonEmpty . M.keys . M.filter (isNothing . threadBlock)

  initialThreadId = fst (nextThreadId initialIdSource)
```

Let's break down that `go` function a bit:

1. We check if the initial thread still exists.  If not, we return.
2. We check if the collection of runnable threads is nonempty.  If
   not, we return.
3. We call the scheduler to pick a thread from the runnable ones.
4. We call the (not yet defined) `stepThread` function to execute one
   step of that thread.
5. We go around the loop again.

Not too bad, hey?  Finally (really finally) we just have one function
to go, `stepThread`.  Can you see what the type will be?

It's going to start like this:

```haskell
stepThread :: C.MonadConc m => ThreadId -> (Threads m, IdSource) -> m (Threads m, IdSource)
stepThread tid (threads, idsrc) = case M.lookup tid threads of
    Just thrd -> go (threadK thrd)
    Nothing -> pure (threads, idsrc)
  where
    adjust :: (Thread m -> Thread m) -> Threads m -> Threads m
    adjust f = M.adjust f tid

    goto :: PrimOp m -> Threads m -> Threads m
    goto k = adjust (\thrd -> thrd { threadK = k })

    block :: Maybe MVarId -> Threads m -> Threads m
    block mv = adjust (\thrd -> thrd { threadBlock = mv })

    unblock :: MVarId -> Threads m -> Threads m
    unblock v = fmap (\thrd ->
      if threadBlock thrd == Just v
      then thrd { threadBlock = Nothing }
      else thrd)

    go :: PrimOp m -> m (Threads m, IdSource)
    -- go ...
```

I've introduced a few helper functions, which will crop up a lot.
That `go` function will have a case for every constructor of `PrimOp
m`, and it's going to look a bit hairy, so we'll take it one
constructor at a time.  Let's do the constructors in order.

First, we can fork threads:

```haskell
    go (Fork (MiniFu ma) k) =
      let (tid', idsrc') = nextThreadId idsrc
          thrd' = thread (K.runCont ma (\_ -> Stop (pure ())))
      in pure (goto (k tid') (M.insert tid' thrd' threads), idsrc')
```

Forking is pretty straightforward.  We simply get the next available
`ThreadId` from the `IdSource`, create a thread with the provided
continuation, and insert it into the `Threads m` map.

Next up is `NewEmptyMVar`:

```haskell
    go (NewEmptyMVar k) = do
      ref <- C.newCRef Nothing
      let (mvid, idsrc') = nextMVarId idsrc
      pure (goto (k (MVar mvid ref)) threads, idsrc')
```

Remember that we're implementing our `MVar` type using the `CRef` type
of the underlying `MonadConc`.  As the `MVar` starts out empty, the
`CRef` starts out holding `Nothing`.

The `PutMVar` and `TakeMVar` actions are almost the same, so let's
tackle them together:

```haskell
    go (PutMVar (MVar mvid ref) a k) = do
      old <- C.readCRef ref
      case old of
        Just _ -> pure (block (Just mvid) threads, idsrc)
        Nothing -> do
          C.writeCRef ref (Just a)
          pure (goto k (unblock mvid threads), idsrc)

    go (TakeMVar (MVar mvid ref) k) = do
      old <- C.readCRef ref
      case old of
        Just a -> do
          C.writeCRef ref Nothing
          pure (goto (k a) (unblock mvid threads), idsrc)
        Nothing -> pure (block (Just mvid) threads, idsrc)
```

In both cases, we start out by reading the value of the reference.
Remember that `Nothing` indicates emptiness, and `Just` indicates the
presence of a value.  So, for `PutMVar` *if there already is a value*
(and for `TakeMVar` *if there isn't a value*), we block the thread.
In the other case, we update the value in the reference, putting in
the new value (or taking out the old), unblock all the relevant
threads, and go to the continuation.

These implementations are not atomic.  But that's fine: despite MiniFu
testing concurrent programs, there's no concurrency going on within
MiniFu itself.  We can do as much or as little as we want during one
atomic "step" of our program.  This will turn out to be very useful
when we implement STM in a few posts time.

Finally, we have `Stop`:

```haskell
    go (Stop mx) = do
      mx
      pure (M.delete tid threads, idsrc)
```

And we're done!  That's it!  All we need now is a scheduler, and we
can execute our example!


A Simple Scheduler
------------------

Our example is nondeterministic, so we want a scheduler which will let
us see that.  It would be no good us implementing something which
always made the same decisions, as we'd only see one result!  So until
we implement the systematic testing behaviour, let's just use a simple
random scheduler.

```haskell
import qualified System.Random as R

randomSched :: R.RandomGen g => Scheduler g
randomSched (t:|ts) g =
  let (i, g') = R.randomR (0, length ts) g
  in ((t:ts) !! i, g')
```

There's no deep magic here, we're just picking a random value from a
nonempty list.  Finally, we can construct a little demo:

```haskell
demo :: IO ()
demo = do
  g <- R.newStdGen
  print . fst =<< minifu randomSched g example
```

Which we can run in ghci like so:

```
λ> demo
Just 1
λ> demo
Just 1
λ> demo
Just 1
λ> demo
Just 2
λ> demo
Just 1
```

Success!

A random scheduler is fine for demonstration purposes, but not so
great for testing.  Different seeds may lead to the same execution,
which makes it hard to know how many executions of a test is enough.
It can be a useful technique, but for us this is only the beginning.


Next time...
------------

Next time we'll look at implementing exceptions, both synchronous and
asynchronous.

I hope you enjoyed this post, any feedback is welcome.  As I mentioned
at the start, this is on [GitHub][], you can get the code we ended up
with at the "post-01" tag.

*Before* next time, I have some homework for you!  You have seen how
to implement MVars, so now try implementing CRefs!  Here are the
functions should you have a go at writing:

```haskell
data CRef m = -- ...

newCRef :: a -> MiniFu m (CRef m a)

readCRef :: CRef m a -> MiniFu m a

writeCRef :: CRef m a -> a -> MiniFu m ()

atomicModifyCRef :: CRef m a -> (a -> (a, b)) -> MiniFu m b
```

Don't worry about any of the relaxed memory stuff implemented in
dejafu, just do sequential consistency (and if you don't know what
that means: it means to do the obvious).  I'll put up a solution (and
maybe do a little refactoring) before the next post.

---

Thanks to [José Manuel Calderón Trilla][jmct] for reading an earlier
draft of this post.

[jmct]: https://twitter.com/josecalderon
