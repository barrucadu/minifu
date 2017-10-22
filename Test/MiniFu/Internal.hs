{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Test.MiniFu.Internal where

import qualified Control.Concurrent.Classy as C
import qualified Control.Exception as E
import qualified Control.Monad.Catch as EM
import qualified Control.Monad.Cont as K
import Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (isNothing)

-- | Threads are just identified by their creation order.
newtype ThreadId = ThreadId Int
  deriving (Eq, Ord)

-- | A scheduler is a stateful function which chooses a thread to run.
type Scheduler s = NonEmpty ThreadId -> s -> (ThreadId, s)

-- | A MiniFu computation is just a continuation over primops.
newtype MiniFu m a = MiniFu { runMiniFu :: K.Cont (PrimOp m) a }
  deriving (Functor, Applicative, Monad)

instance EM.MonadThrow (MiniFu m) where
  throwM e = MiniFu (K.cont (\_ -> Throw e))

instance EM.MonadCatch (MiniFu m) where
  catch act h = MiniFu (K.cont (Catch act h))

-- | One of the basic actions that a @MonadConc@ can do.
data PrimOp m where
  -- threading
  Fork :: MiniFu m () -> (ThreadId -> PrimOp m) -> PrimOp m
  -- mvars
  NewEmptyMVar :: (MVar m a -> PrimOp m) -> PrimOp m
  PutMVar :: MVar m a -> a -> PrimOp m -> PrimOp m
  ReadMVar :: MVar m a -> (a -> PrimOp m) -> PrimOp m
  TakeMVar :: MVar m a -> (a -> PrimOp m) -> PrimOp m
  -- crefs
  NewCRef :: a -> (CRef m a -> PrimOp m) -> PrimOp m
  ReadCRef :: CRef m a -> (a -> PrimOp m) -> PrimOp m
  WriteCRef :: CRef m a -> a -> PrimOp m -> PrimOp m
  ModifyCRef :: CRef m a -> (a -> (a, b)) -> (b -> PrimOp m) -> PrimOp m
  -- exceptions
  Throw :: E.Exception e => e -> PrimOp m
  Catch :: E.Exception e => MiniFu m a -> (e -> MiniFu m a) -> (a -> PrimOp m) -> PrimOp m
  PopH  :: PrimOp m -> PrimOp m
  Mask  :: E.MaskingState -> PrimOp m -> PrimOp m
  -- misc
  Stop :: m () -> PrimOp m

-- | @MVar@s have a unique ID too, used in thread blocking.
newtype MVarId = MVarId Int
  deriving (Eq, Ord)

-- | An @MVar@ is a @CRef@ in the underlying monad, holding a maybe
-- value, with a unique identifier.
data MVar m a = MVar
  { mvarId  :: MVarId
  , mvarRef :: C.CRef m (Maybe a)
  }

-- | A @CRef@ just delegates directly to the underlying monad.
newtype CRef m a = CRef { crefRef :: C.CRef m a }

-------------------------------------------------------------------------------

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
    block  v = adjust (\thrd -> thrd { threadBlock = Just v })
    unblock v = fmap (\thrd ->
      if threadBlock thrd == Just v
      then thrd { threadBlock = Nothing }
      else thrd)
    simple f = pure (f threads, idsrc)

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
        Just _ -> simple (block (Right mvid))
        Nothing -> do
          C.writeCRef ref (Just a)
          simple (goto k . unblock (Right mvid))
    go (TakeMVar (MVar mvid ref) k) = do
      old <- C.readCRef ref
      case old of
        Just a -> do
          C.writeCRef ref Nothing
          simple (goto (k a) . unblock (Right mvid))
        Nothing -> simple (block (Right mvid))
    go (ReadMVar (MVar mvid ref) k) = do
      cur <- C.readCRef ref
      simple $ maybe (block (Right mvid)) (goto . k) cur
    go (NewCRef a k) = do
      ref <- C.newCRef a
      simple (goto (k (CRef ref)))
    go (ReadCRef (CRef ref) k) = do
      cur <- C.readCRef ref
      simple (goto (k cur))
    go (WriteCRef (CRef ref) a k) = do
      C.writeCRef ref a
      simple (goto k)
    go (ModifyCRef (CRef ref) f k) = do
      new <- C.atomicModifyCRef ref f
      simple (goto (k new))
    go (Throw e) =
      simple (M.update (raise e) tid)
    go (Catch (MiniFu ma) h k) = simple . adjust $ \thrd -> thrd
      { threadK   = K.runCont ma (PopH . k)
      , threadExc =
        let h' exc = flip K.runCont k $ do
              K.cont (\c -> Mask (threadMask thrd) (c ()))
              runMiniFu (h exc)
        in Handler h' : threadExc thrd
      }
    go (PopH k) = simple . adjust $ \thrd -> thrd
      { threadK   = k
      , threadExc = tail (threadExc thrd)
      }
    go (Mask ms k) = simple . adjust $ \thrd -> thrd
      { threadK    = k
      , threadMask = ms
      }
    go (Stop mx) = do
      mx
      simple (M.delete tid)

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
  , threadBlock :: Maybe (Either ThreadId MVarId)
  , threadExc   :: [Handler m]
  , threadMask  :: E.MaskingState
  }

-- | An exception handler.
data Handler m where
  Handler :: E.Exception e => (e -> PrimOp m) -> Handler m

-- | Create a new thread
thread :: PrimOp m -> Thread m
thread k = Thread
  { threadK     = k
  , threadBlock = Nothing
  , threadExc   = []
  , threadMask  = E.Unmasked
  }

-- | Create the initial thread and ID source
initialise :: PrimOp m -> (Threads m, IdSource)
initialise pop =
  let (tid, idsrc) = nextThreadId initialIdSource
  in (M.singleton tid (thread pop), idsrc)

-- | Raise an exception in a thread.  If this returns @Nothing@ the
-- thread has been killed.
raise :: E.Exception e => e -> Thread m -> Maybe (Thread m)
raise exc thrd = go (threadExc thrd) where
  go (Handler h:hs) = case h <$> E.fromException exc' of
    Just pop -> Just (thrd { threadK = pop, threadBlock = Nothing, threadExc = hs })
    Nothing  -> go hs
  go [] = Nothing

  exc' = E.toException exc
