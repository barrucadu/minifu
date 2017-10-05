{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Test.MiniFu where

import qualified Control.Concurrent.Classy as C
import qualified Control.Monad.Cont as K
import Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (isNothing)

example :: MiniFu m Int
example = do
  a <- newEmptyMVar
  fork (putMVar a 1)
  fork (putMVar a 2)
  takeMVar a

-------------------------------------------------------------------------------

-- | Threads are just identified by their creation order.
newtype ThreadId = ThreadId Int
  deriving (Eq, Ord)

-- | A scheduler is a stateful function which chooses a thread to run.
type Scheduler s = NonEmpty ThreadId -> s -> (ThreadId, s)

-- | A MiniFu computation is just a continuation over primops.
newtype MiniFu m a = MiniFu { runMiniFu :: K.Cont (PrimOp m) a }
  deriving (Functor, Applicative, Monad)

-- | One of the basic actions that a @MonadConc@ can do.
data PrimOp m where
  Fork         :: MiniFu m () -> (ThreadId -> PrimOp m) -> PrimOp m
  NewEmptyMVar :: (MVar m a -> PrimOp m)                -> PrimOp m
  PutMVar      :: MVar m a -> a -> PrimOp m             -> PrimOp m
  TakeMVar     :: MVar m a -> (a -> PrimOp m)           -> PrimOp m
  Stop         :: m ()                                  -> PrimOp m

-- | @MVar@s have a unique ID too, used in thread blocking.
newtype MVarId = MVarId Int
  deriving (Eq, Ord)

-- | An @MVar@ is a @CRef@ in the underlying monad, holding a maybe
-- value, with a unique identifier.
data MVar m a = MVar
  { mvarId  :: MVarId
  , mvarRef :: C.CRef m (Maybe a)
  }

-- | Fork a computation to happen concurrently.
fork :: MiniFu m () -> MiniFu m ThreadId
fork ma = MiniFu (K.cont (Fork ma))

-- | Create a new empty @MVar@.
newEmptyMVar :: MiniFu m (MVar m a)
newEmptyMVar = MiniFu (K.cont NewEmptyMVar)

-- | Put a value into a @MVar@. If there is already a value there,
-- this will block until that value has been taken, at which point the
-- value will be stored.
putMVar :: MVar m a -> a -> MiniFu m ()
putMVar v a = MiniFu (K.cont (\k -> PutMVar v a (k ())))

-- | Take a value from a @MVar@. This "empties" the @MVar@, allowing a
-- new value to be put in. This will block if there is no value in the
-- @MVar@ already, until one has been put.
takeMVar :: MVar m a -> MiniFu m a
takeMVar v = MiniFu (K.cont (TakeMVar v))

-------------------------------------------------------------------------------

-- | Execute a concurrent computation with a given scheduler, and
-- return the result.
minifu :: C.MonadConc m => Scheduler s -> s -> MiniFu m a -> m (Maybe a, s)
minifu sched s (MiniFu ma) = do
  out <- C.newCRef Nothing
  s'  <- run sched s (K.runCont ma (Stop . C.writeCRef out . Just))
  a   <- C.readCRef out
  pure (a, s')

-- | Run a collection of threads to completion.
run :: C.MonadConc m => Scheduler s -> s -> PrimOp m -> m s
run sched s0 = go s0 . initialise where
  go s (threads, idsrc)
    | initialThreadId `M.member` threads = case runnable threads of
      Just tids ->
        let (chosen, s') = sched tids s
        in go s' =<< stepThread chosen (threads, idsrc)
      Nothing -> pure s
    | otherwise = pure s

  runnable = nonEmpty . M.keys . M.filter (isNothing . threadBlock)

  initialThreadId = fst (nextThreadId initialIdSource)

stepThread :: C.MonadConc m => ThreadId -> (Threads m, IdSource) -> m (Threads m, IdSource)
stepThread tid (threads, idsrc) = case M.lookup tid threads of
    Just thrd -> go (threadK thrd)
    Nothing -> pure (threads, idsrc)
  where
    adjust f = M.adjust f tid
    goto   k = adjust (\thrd -> thrd { threadK = k })
    block mv = adjust (\thrd -> thrd { threadBlock = mv })
    unblock v = fmap (\thrd ->
      if threadBlock thrd == Just v
      then thrd { threadBlock = Nothing }
      else thrd)

    go (Fork (MiniFu ma) k) =
      let (tid', idsrc') = nextThreadId idsrc
          thrd' = thread (K.runCont ma (\_ -> Stop (pure ())))
      in pure (goto (k tid') (M.insert tid' thrd' threads), idsrc')
    go (NewEmptyMVar k) = do
      ref <- C.newCRef Nothing
      let (mvid, idsrc') = nextMVarId idsrc
      pure (goto (k (MVar mvid ref)) threads, idsrc')
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
    go (Stop mx) = do
      mx
      pure (M.delete tid threads, idsrc)

-------------------------------------------------------------------------------

-- | An identifier source is a simple counter.
type IdSource = Int

-- | Create an identifier source.
initialIdSource :: IdSource
initialIdSource = 0

-- | Get a new unique thread ID.
nextThreadId :: IdSource -> (ThreadId, IdSource)
nextThreadId n = (ThreadId n, n + 1)

-- | Get a new unique @MVar@ ID:
nextMVarId :: IdSource -> (MVarId, IdSource)
nextMVarId n = (MVarId n, n + 1)

-------------------------------------------------------------------------------

-- | A collection of threads is just a map of thread records keyed by
-- ID.
type Threads m = Map ThreadId (Thread m)

-- | A thread is a continuation along with what @MVar@ it is blocked
-- on.
data Thread m = Thread
  { threadK     :: PrimOp m
  , threadBlock :: Maybe MVarId
  }

-- | Create a new thread
thread :: PrimOp m -> Thread m
thread k = Thread
  { threadK     = k
  , threadBlock = Nothing
  }

-- | Create the initial thread and ID source
initialise :: PrimOp m -> (Threads m, IdSource)
initialise pop =
  let (tid, idsrc) = nextThreadId initialIdSource
  in (M.singleton tid (thread pop), idsrc)
