{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Test.MiniFu.Internal where

import qualified Control.Concurrent.Classy as C
import qualified Control.Exception as E
import Control.Monad (when)
import qualified Control.Monad.Catch as EM
import qualified Control.Monad.Cont as K
import Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust, isNothing)

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

instance EM.MonadMask (MiniFu m) where
  mask ma = MiniFu (K.cont (InMask E.MaskedInterruptible ma))
  uninterruptibleMask ma = MiniFu (K.cont (InMask E.MaskedUninterruptible ma))

-- | One of the basic actions that a @MonadConc@ can do.
data PrimOp m where
  -- threading
  Fork :: MiniFu m () -> (ThreadId -> PrimOp m) -> PrimOp m
  -- mvars
  NewEmptyMVar :: (MVar m a -> PrimOp m) -> PrimOp m
  PutMVar :: MVar m a -> a -> PrimOp m -> PrimOp m
  ReadMVar :: MVar m a -> (a -> PrimOp m) -> PrimOp m
  TakeMVar :: MVar m a -> (a -> PrimOp m) -> PrimOp m
  TryPutMVar :: MVar m a -> a -> (Bool -> PrimOp m) -> PrimOp m
  TryReadMVar :: MVar m a -> (Maybe a -> PrimOp m) -> PrimOp m
  TryTakeMVar :: MVar m a -> (Maybe a -> PrimOp m) -> PrimOp m
  -- crefs
  NewCRef :: a -> (CRef m a -> PrimOp m) -> PrimOp m
  ReadCRef :: CRef m a -> (a -> PrimOp m) -> PrimOp m
  WriteCRef :: CRef m a -> a -> PrimOp m -> PrimOp m
  ModifyCRef :: CRef m a -> (a -> (a, b)) -> (b -> PrimOp m) -> PrimOp m
  -- exceptions
  Throw :: E.Exception e => e -> PrimOp m
  ThrowTo :: E.Exception e => ThreadId -> e -> PrimOp m -> PrimOp m
  Catch :: E.Exception e => MiniFu m a -> (e -> MiniFu m a) -> (a -> PrimOp m) -> PrimOp m
  PopH  :: PrimOp m -> PrimOp m
  Mask  :: E.MaskingState -> PrimOp m -> PrimOp m
  InMask :: E.MaskingState -> ((forall x. MiniFu m x -> MiniFu m x) -> MiniFu m a) -> (a -> PrimOp m) -> PrimOp m
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

stepThread :: C.MonadConc m => ThreadId -> (Threads m, IdSource) -> m (Threads m, IdSource)
stepThread tid (threads, idsrc) = case M.lookup tid threads of
    Just thrd -> go (threadK thrd)
    Nothing -> pure (threads, idsrc)
  where
    adjust f = M.adjust f tid
    goto   k = adjust (\thrd -> thrd { threadK = k })
    block  v = adjust (\thrd -> thrd { threadBlock = Just v })
    simple f = pure (f threads, idsrc)

    go (Fork (MiniFu ma) k) =
      let (tid', idsrc') = nextThreadId idsrc
          thrd' = thread (K.runCont ma (\_ -> Stop (pure ())))
      in pure (goto (k tid') (M.insert tid' thrd' threads), idsrc')
    go (NewEmptyMVar k) = do
      ref <- C.newCRef Nothing
      let (mvid, idsrc') = nextMVarId idsrc
      pure (goto (k (MVar mvid ref)) threads, idsrc')
    go (PutMVar mvar a k) =
      simple . ($tid) =<< putIntoMVar Blocking mvar a (const k)
    go (TakeMVar mvar k) =
      simple . ($tid) =<< seeIntoMVar Blocking Emptying mvar (k . fromJust)
    go (ReadMVar mvar k) =
      simple . ($tid) =<< seeIntoMVar Blocking NonEmptying mvar (k . fromJust)
    go (TryPutMVar mvar a k) =
      simple . ($tid) =<< putIntoMVar NonBlocking mvar a k
    go (TryTakeMVar mvar k) =
      simple . ($tid) =<< seeIntoMVar NonBlocking Emptying mvar k
    go (TryReadMVar mvar k) =
      simple . ($tid) =<< seeIntoMVar NonBlocking NonEmptying mvar k
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
    go (ThrowTo threadid e k) = simple $ case M.lookup threadid threads of
      Just t
        | isInterruptible t -> goto k . M.update (raise e) threadid
        | otherwise         -> block (Left threadid)
      Nothing -> goto k
    go (Catch (MiniFu ma) h k) = simple . adjust $ \thrd -> thrd
      { threadK   = K.runCont ma (PopH . k)
      , threadExc =
        let ms0 = threadMask thrd
            h' exc = flip K.runCont k $ do
              K.cont (\c -> Mask ms0 (c ()))
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

-- | Check if a thread is interruptible by an asynchronous exception.
isInterruptible :: Thread m -> Bool
isInterruptible thrd =
  threadMask thrd == E.Unmasked ||
  (threadMask thrd == E.MaskedInterruptible && isJust (threadBlock thrd))

-- | Block a thread.
block :: Either ThreadId MVarId -> ThreadId -> Threads m -> Threads m
block v = M.adjust (\thrd -> thrd { threadBlock = Just v })

-- | Unblock all matching threads.
unblock :: Functor f => Either ThreadId MVarId -> f (Thread m) -> f (Thread m)
unblock v = fmap $ \thrd ->
  if threadBlock thrd == Just v
  then thrd { threadBlock = Nothing }
  else thrd

-- | Change the continuation of a thread.
goto :: PrimOp m -> ThreadId -> Threads m -> Threads m
goto k = M.adjust (\thrd -> thrd { threadK = k })

-------------------------------------------------------------------------------

data Blocking = Blocking | NonBlocking
data Emptying = Emptying | NonEmptying deriving Eq

-- | Abstraction over @putMVar@ and @tryPutMVar@
putIntoMVar :: C.MonadConc m
  => Blocking
  -> MVar m a
  -> a
  -> (Bool -> PrimOp m)
  -> m (ThreadId -> Threads m -> Threads m)
putIntoMVar blocking (MVar mvid ref) a k = do
  old <- C.readCRef ref
  case old of
    Just _ -> pure $ case blocking of
      Blocking    -> block (Right mvid)
      NonBlocking -> goto (k False)
    Nothing -> do
      C.writeCRef ref (Just a)
      pure $ \tid -> goto (k True) tid . unblock (Right mvid)

-- | Abstraction over @readMVar@, @takeMVar@, @tryReadMVar@, and
-- @tryTakeMVar@.
seeIntoMVar :: C.MonadConc m
  => Blocking
  -> Emptying
  -> MVar m a
  -> (Maybe a -> PrimOp m)
  -> m (ThreadId -> Threads m -> Threads m)
seeIntoMVar blocking emptying (MVar mvid ref) k = do
  old <- C.readCRef ref
  case old of
    Just a -> do
      when (emptying == Emptying) (C.writeCRef ref Nothing)
      pure $ \tid -> goto (k (Just a)) tid . unblock (Right mvid)
    Nothing -> pure $ case blocking of
      Blocking    -> block (Right mvid)
      NonBlocking -> goto (k Nothing)
