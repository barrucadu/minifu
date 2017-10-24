Writing a Concurrency Testing Library (Part 2): Exceptions
==========================================================

Welcome back to my series on implementing a concurrency testing
library for Haskell.  This is part 2 of the series, and today we'll
implement exceptions.  If you missed part 1, you can read it [here][].

As before, all code is available on [GitHub][].  The code for this
post is under the "post-02" tag.

[here]: https://www.barrucadu.co.uk/posts/concurrency/2017-10-14-writing-a-concurrency-testing-library-01.html
[GitHub]: https://github.com/barrucadu/minifu

---

Did you do last time's homework task?  It was to implement this
interface:

```haskell
data CRef m a = -- ...

newCRef :: a -> MiniFu m (CRef m a)

readCRef :: CRef m a -> MiniFu m a

writeCRef :: CRef m a -> a -> MiniFu m ()

atomicModifyCRef :: CRef m a -> (a -> (a, b)) -> MiniFu m b
```

Here are my solutions, available at the "homework-01" tag:

1. ([`2070bdf`][]) Add the `CRef` type, the `PrimOp` constructors, and the wrapper functions
2. ([`188eec5`][]) Implement the primops

[`2070bdf`]: https://github.com/barrucadu/minifu/commit/2070bdfaf5174fc14f6835d8410988cf111a854a
[`188eec5`]: https://github.com/barrucadu/minifu/commit/188eec562f619c26fe117dd891ff86befc27b5a2

I also made some changes, available at the "pre-02" tag:

1. ([`7ce6e41`][]) Add a helper for primops which don't create any identifiers
2. ([`2419796`][]) Move some definitions into an internal module
3. ([`9c49f9d`][]) Change the type of the `block` helper to `MVarId -> Threads m -> Threads m`
4. ([`dabd84b`][]) Implement `readMVar`

[`7ce6e41`]: https://github.com/barrucadu/minifu/commit/7ce6e41f8bdc60c73affa00f7760a46a7e6ecfc3
[`2419796`]: https://github.com/barrucadu/minifu/commit/24197965787555c5552ce8cb70fcb078016a167c
[`9c49f9d`]: https://github.com/barrucadu/minifu/commit/9c49f9d76f27ce0fa1ed445c34d9107105e66171
[`dabd84b`]: https://github.com/barrucadu/minifu/commit/dabd84b1ed4f713889b607b142ecb2d1987ee804

Now on to the show...


Synchronous exceptions
----------------------

We can't implement exceptions with what we have already.  We're going
to need some new primops.  I think you're getting a feel for how this
works now, so I won't drag this out.  Here we go:

```haskell
import qualified Control.Exception as E

data PrimOp m where
  -- ...
  Throw :: E.Exception e => e -> PrimOp m
  Catch :: E.Exception e => MiniFu m a -> (e -> MiniFu m a) -> (a -> PrimOp m) -> PrimOp m
  PopH  :: PrimOp m -> PrimOp m

throw :: E.Exception e => e -> MiniFu m a
throw e = MiniFu (K.cont (\_ -> Throw e))

catch :: E.Exception e => MiniFu m a -> (e -> MiniFu m a) -> MiniFu m a
catch act h = MiniFu (K.cont (Catch act h))
```

Throwing an exception with `throw` jumps back to the closest enclosing
`catch` with an exception handler of the appropriate type, killing the
thread if there is none.  The `PopH` primop will pop the top exception
handler from the stack.  We'll insert those as appropriate when
entering a `catch`.

Before we can actually implement these primops, we need to give
threads a place to store their exception handlers.  You might have
guessed it when I said "stack": we'll just give every thread a list of
them.  This requires changing our `Thread` type and `thread` function:

```haskell
data Thread m = Thread
  { threadK     :: PrimOp m
  , threadBlock :: Maybe MVarId
  , threadExc   :: [Handler m]
  }

data Handler m where
  Handler :: E.Exception e => (e -> PrimOp m) -> Handler m

thread :: PrimOp m -> Thread m
thread k = Thread
  { threadK     = k
  , threadBlock = Nothing
  , threadExc   = []
  }
```

As `Exception` is a subclass of `Typeable`, given some exception value
we're able to look for the first matching handler:

```haskell
raise :: E.Exception e => e -> Thread m -> Maybe (Thread m)
raise exc thrd = go (threadExc thrd) where
  go (Handler h:hs) = case h <$> E.fromException exc' of
    Just pop -> Just (thrd { threadK = pop, threadBlock = Nothing, threadExc = hs })
    Nothing  -> go hs
  go [] = Nothing

  exc' = E.toException exc
```

If `raise` returns a `Just`, then a handler was found and entered.
Otherwise, no handler exists and the thread should be removed from the
`Threads` collection.  This can be expressed rather nicely as
`M.update . raise`.

Now we have enough support to implement the primops:

```haskell
stepThread {- ... -}
  where
    -- ...
    go (Throw e) =
      simple (M.update (raise e) tid)
    go (Catch (MiniFu ma) h k) = simple . adjust $ \thrd -> thrd
      { threadK   = K.runCont ma (PopH . k)
      , threadExc =
        let h' exc = K.runCont (runMiniFu (h exc)) k
        in Handler h' : threadExc thrd
      }
    go (PopH k) = simple . adjust $ \thrd -> thrd
      { threadK   = k
      , threadExc = tail (threadExc thrd)
      }
```

Let's break that down:

- `Throw` just re-uses our `raise` function to either jump to the
  exception handler or kill the thread.
- `Catch` changes the continuation of the thread to run the enclosed
  action, then do a `PopH` action, then run the outer action.  It also
  adds an exception continuation, which just runs the exception
  handler, then runs the outer action.
- `PopH` just removes the head exception continuation.

It's important that the exception continuation *doesn't* use `PopH` to
remove itself: that happens in `raise` when an exception is thrown.
When writing this section I realised I'd made that mistake in dejafu
([#139][])!

[#139]: https://github.com/barrucadu/dejafu/issues/139

### So what?

So now we can use synchronous exceptions!  Here's an incredibly
contrived example:

```haskell
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad (join)

example_sync :: MiniFu m Int
example_sync = do
  a <- newEmptyMVar
  fork (putMVar a (pure 1))
  fork (putMVar a (throw E.NonTermination))
  fork (putMVar a (throw E.AllocationLimitExceeded))
  catch
    (catch
      (join (readMVar a))
      (\(_ :: E.AllocationLimitExceeded) -> pure 2))
    (\(_ :: E.NonTermination) -> pure 3)

demo_sync :: IO ()
demo_sync = do
  g <- R.newStdGen
  print . fst =<< minifu randomSched g example_sync
```

If we run this a few times in ghci, we can see the different
exceptions being thrown and caught (resulting in different outputs):

```
λ> demo_sync
Just 1
λ> demo_sync
Just 3
λ> demo_sync
Just 3
λ> demo_sync
Just 2
```

### MonadThrow and MonadCatch

`MonadConc` has a bunch of superclasses, and we can now implement two
of them!

```haskell
import qualified Control.Monad.Catch as EM

instance EM.MonadThrow (MiniFu m) where
  throwM = -- 'throw' from above

instance EM.MonadCatch (MiniFu m) where
  catch = -- 'catch' from above
```

The [exceptions][] package provides the `MonadThrow`, `MonadCatch`,
and `MonadMask` typeclasses, so we can talk about exceptions in a
wider context than just `IO`.  We'll get on to `MonadMask` when we
look at asynchronous exceptions.

[exceptions]: https://hackage.haskell.org/package/exceptions

### Incompleteness!

It is with exceptions that we hit the first thing we can't do in
MiniFu.

When in `IO`, we can catch exceptions from pure code:

```
λ> import Control.Exception
λ> evaluate undefined `catch` \e -> putStrLn ("Got " ++ show (e :: SomeException))
Got Prelude.undefined
CallStack (from HasCallStack):
  error, called at libraries/base/GHC/Err.hs:79:14 in base:GHC.Err
  undefined, called at <interactive>:5:10 in interactive:Ghci2
```

But we can't do that in `MiniFu`, as there's no suitable `evaluate`
function.

Should there be an `evaluate` in the `MonadConc` class?  I'm
unconvinced, as it's not really a *concurrency* operation.

Should we constrain the `m` in `MiniFu m` to be a `MonadIO`, which
would let us call `evaluate`?  Perhaps, that would certainly be a way
to do it, and I'm currently investigating the advantages of an `IO`
base monad for dejafu (although originally for a different reason).


Asynchronous exceptions
-----------------------

Asynchronous exceptions are like synchronous exceptions, except for
two details:

1. They are raised in a thread identified by `ThreadId`.  We can do
   this already with `raise`.
2. Raising the exception may be blocked due to the target thread's
   *masking state*.  We need to do some extra work to implement this.

When a thread is masked, attempting to deliver an asynchronous
exception to it will block.  There are three masking states:

- `Unmasked`, asynchronous exceptions are unmasked.
- `MaskedInterruptible`, asynchronous exceptions are masked, but
  blocked operations may still be interrupted.
- `MaskedUninterruptible`, asynchronous exceptions are masked, and
  blocked operations may not be interrupted.

So we'll add the current masking state to our `Thread` type,
defaulting to `Unmasked`, and also account for blocking on another
thread:

```haskell
data Thread m = Thread
  { threadK     :: PrimOp m
  , threadBlock :: Maybe (Either ThreadId MVarId)
  , threadExc   :: [Handler m]
  , threadMask  :: E.MaskingState
  }

thread :: PrimOp m -> Thread m
thread k = Thread
  { threadK     = k
  , threadBlock = Nothing
  , threadExc   = []
  , threadMask  = E.Unmasked
  }
```

We'll also need a primop to set the masking state:

```haskell
data PrimOp m where
  -- ...
  Mask :: E.MaskingState -> PrimOp m -> PrimOp m
```

Which has a fairly straightforward implementation:

```haskell
stepThread {- ... -}
  where
    -- ...
    go (Mask ms k) = simple . adjust $ \thrd -> thrd
      { threadK    = k
      , threadMask = ms
      }
```

Finally, we need to make sure that if an exception is raised, and we
jump into an exception handler, the masking state gets reset to what
it was when the handler was created.  This means we need a small
change to the `Catch` primop:

```haskell
stepThread {- ... -}
  where
    -- ...
    go (Catch (MiniFu ma) h k) = simple . adjust $ \thrd -> thrd
      { threadK   = K.runCont ma (PopH . k)
      , threadExc =
        let h' exc = flip K.runCont k $ do
              K.cont (\c -> Mask (threadMask thrd) (c ()))
              runMiniFu (h exc)
        in Handler h' : threadExc thrd
      }
```

Alright, now we have enough background to actually implement the
user-facing operations.

### Throwing

To throw an asynchronous exception, we're going to need a new primop:

```haskell
data PrimOp m where
  -- ...
  ThrowTo :: E.Exception e => ThreadId -> e -> PrimOp m -> PrimOp m
```

Which has a corresponding wrapper function:

```haskell
throwTo :: E.Exception e => ThreadId -> e -> MiniFu m ()
throwTo tid e = MiniFu (K.cont (\k -> ThrowTo tid e (k ())))
```

Let's think about the implementation of the `ThrowTo` primop.  It
first needs to check if the target thread is interruptible and, if so,
raises the exception in that thread; if not, it blocks the current
thread.  A thread is interruptible if its masking state is `Unmasked`,
or `MaskedInterruptible` and it's currently blocked.

Let's encapsulate that logic:

```haskell
import Data.Maybe (isJust)

isInterruptible :: Thread m -> Bool
isInterruptible thrd =
  threadMask thrd == E.Unmasked ||
  (threadMask thrd == E.MaskedInterruptible && isJust (threadBlock thrd))
```

Given that, the implementation of `ThrowTo` is straightforward:

```haskell
stepThread {- ... -}
  where
    -- ...
    go (ThrowTo threadid e k) = simple $ case M.lookup threadid threads of
      Just t
        | isInterruptible t -> goto k . M.update (raise e) threadid
        | otherwise         -> block (Left threadid)
      Nothing -> goto k
```

First, check if the thread exists.  Then check if it's interruptible:
if it is, raise the exception, otherwise block.  If the thread doesn't
exist any more, just continue.

Now we just need to handle *unblocking* threads which are blocked in
`ThrowTo`.  For that, we'll go back to the `run` function and add a
pass to unblock threads if the current one is interruptible after it
processes its action:

```haskell
run :: C.MonadConc m => Scheduler s -> s -> PrimOp m -> m s
run sched s0 = go s0 . initialise where
  go s (threads, idsrc)
    | initialThreadId `M.member` threads = case runnable threads of
      Just tids -> do
        let (chosen, s') = sched tids s
        (threads', idsrc') <- stepThread chosen (threads, idsrc)
        let threads'' = if (isInterruptible <$> M.lookup chosen threads') /= Just False
                        then unblock (Left chosen) threads'
                        else threads'
        go s' (threads'', idsrc')
      Nothing -> pure s
    | otherwise = pure s

  runnable = nonEmpty . M.keys . M.filter (isNothing . threadBlock)

  initialThreadId = fst (nextThreadId initialIdSource)
```

So after stepping a thread, we unblock every thread blocked on it if
it either doesn't exist, of if it does exist and is interruptible.
It's much more robust to do this once here than everywhere in
`stepThread` which might cause the thread to become interruptible.

### Masking and MonadMask

There are two operations at the programmer's disposal to change the
masking state of a thread, `mask` and `uninterruptibleMask`.  Here's
what the `MiniFu` types will look like:

```haskell
{-# LANGUAGE RankNTypes #-}

mask                :: ((forall x. MiniFu m x -> MiniFu m x) -> MiniFu m a) -> MiniFu m a
uninterruptibleMask :: ((forall x. MiniFu m x -> MiniFu m x) -> MiniFu m a) -> MiniFu m a
```

Each takes an action to run, and runs it as either
`MaskedInterruptible` or `MaskedUninterruptible`.  The action is
provided with a polymorphic callback to run a subcomputation with the
original masking state.

This is going to need, you guessed it, a new primop!  We *could*
modify the `Mask` primop to do this job as well, but I think it's a
little clearer to have two separate ones:

```haskell
data PrimOp m where
  -- ...
  InMask :: E.MaskingState -> ((forall x. MiniFu m x -> MiniFu m x) -> MiniFu m a) -> (a -> PrimOp m) -> PrimOp m
```

And here's the implementations of our masking functions:

```haskell
mask ma = MiniFu (K.cont (InMask E.MaskedInterruptible ma))
uninterruptibleMask ma = MiniFu (K.cont (InMask E.MaskedUninterruptible ma))
```

We can now fulfil another requirement of `MonadConc`: a `MonadMask`
instance!

```haskell
instance MonadMask (MiniFu m) where
  mask = -- 'mask' from above
  uninterruptibleMask = -- 'uninterruptibleMask' from above
```

The very last piece of the puzzle for exception handling in MiniFu is
to implement this `InMask` primop.  The type looks quite intense, but
it's really not that bad.  There are three parts: (1) we need to
construct the polymorphic argument function; (2) we need to change the
masking state; and (3) we need to run the inner continuation,
resetting the masking state when done.

```haskell
stepThread {- ... -}
  where
    -- ...
    go (InMask ms ma k) = simple . adjust $ \thrd -> thrd
      { threadK =
        let ms0 = threadMask thrd
            umask :: MiniFu m x -> MiniFu m x
            umask (MiniFu mx) = MiniFu $ do
              K.cont (\c -> Mask ms0 (c ()))
              x <- mx
              K.cont (\c -> Mask ms (c ()))
              pure x
        in K.runCont (runMiniFu (ma umask)) (Mask ms0 . k)
      , threadMask = ms
      }
```

The explicit type signature on `umask` is needed because we're using
`GADTs`, which implies `MonoLocalBinds`, which prevents the
polymorphic type from being inferred.  We could achieve the same
effect by turning on `NoMonoLocalBinds`.

### Demo

Now we have asynchronous exceptions, check it out:

```haskell
example_async :: MiniFu m String
example_async = do
  a <- newEmptyMVar
  tid <- fork (putMVar a "hello from the other thread")
  throwTo tid E.ThreadKilled
  readMVar a

demo_async :: IO ()
demo_async = do
  g <- R.newStdGen
  print . fst =<< minifu randomSched g example_async
```

See:

```
λ> demo_async
Just "hello from the other thread"
λ> demo_async
Just "hello from the other thread"
λ> demo_async
Nothing
```

Next time...
------------

We have come to the end of part 2!  Again, I hope you enjoyed this
post, any feedback is welcome.  This is all on [GitHub][], and you can
see the code we ended up with at the "post-02" tag.

Once again, I have some homework for you.  Your task, should you
choose to accept it, is to implement:

```haskell
tryPutMVar :: MVar m a -> a -> MiniFu m Bool

tryTakeMVar :: MVar m a -> MiniFu m (Maybe a)

tryReadMVar :: MVar m a -> MiniFu m (Maybe a)
```

Solutions will be up in a few days, as before, at the "homework-02"
tag.

Stay tuned because next time we're going to implement STM: all of it
in one go.  Then we can finally get on to the testing.

---

Thanks to [name][] for reading and earlier draft of this post.

[name]: url
