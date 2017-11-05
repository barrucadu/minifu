{-# LANGUAGE RankNTypes #-}
module Test.MiniFu
  ( module Test.MiniFu
  -- * Re-exports
  , MiniFu, ThreadId, MVar, CRef, Scheduler
  ) where

import qualified Control.Concurrent.Classy as C
import qualified Control.Exception as E
import qualified Control.Monad.Cont as K
import Data.List.NonEmpty (NonEmpty(..))
import qualified System.Random as R

import Test.MiniFu.Internal

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

-- | Attempt to put a value in a @MVar@ non-blockingly, returning
-- 'True' (and filling the @MVar@) if there was nothing there,
-- otherwise returning 'False'.
tryPutMVar :: MVar m a -> a -> MiniFu m Bool
tryPutMVar v a = MiniFu (K.cont (TryPutMVar v a))

-- | Block until a value is present in the @MVar@, and then return
-- it. This does not "remove" the value, multiple reads are possible.
readMVar :: MVar m a -> MiniFu m a
readMVar v = MiniFu (K.cont (ReadMVar v))

-- | Attempt to read a value from a @MVar@ non-blockingly, returning a
-- 'Just' if there is something there, otherwise returning
-- 'Nothing'. As with 'readMVar', this does not \"remove\" the value.
tryReadMVar :: MVar m a -> MiniFu m (Maybe a)
tryReadMVar v = MiniFu (K.cont (TryReadMVar v))

-- | Take a value from a @MVar@. This "empties" the @MVar@, allowing a
-- new value to be put in. This will block if there is no value in the
-- @MVar@ already, until one has been put.
takeMVar :: MVar m a -> MiniFu m a
takeMVar v = MiniFu (K.cont (TakeMVar v))

-- | Attempt to take a value from a @MVar@ non-blockingly, returning a
-- 'Just' (and emptying the @MVar@) if there was something there,
-- otherwise returning 'Nothing'.
tryTakeMVar :: MVar m a -> MiniFu m (Maybe a)
tryTakeMVar v = MiniFu (K.cont (TryTakeMVar v))

-- | Create a new reference.
newCRef :: a -> MiniFu m (CRef m a)
newCRef a = MiniFu (K.cont (NewCRef a))

-- | Read the current value stored in a reference.
readCRef :: CRef m a -> MiniFu m a
readCRef r = MiniFu (K.cont (ReadCRef r))

-- | Write a new value into an @CRef@.
writeCRef :: CRef m a -> a -> MiniFu m ()
writeCRef r a = MiniFu (K.cont (\k -> WriteCRef r a (k ())))

-- | Atomically modify the value stored in a reference.
atomicModifyCRef :: CRef m a -> (a -> (a, b)) -> MiniFu m b
atomicModifyCRef r f = MiniFu (K.cont (ModifyCRef r f))

-- | Throw an exception. This will \"bubble up\" looking for an
-- exception handler capable of dealing with it and, if one is not
-- found, the thread is killed.
throw :: E.Exception e => e -> MiniFu m a
throw e = MiniFu (K.cont (\_ -> Throw e))

-- | Catch an exception raised by 'throw'.
catch :: E.Exception e => MiniFu m a -> (e -> MiniFu m a) -> MiniFu m a
catch act h = MiniFu (K.cont (Catch act h))

-- | Throw an exception to the target thread. This blocks until the
-- exception is delivered.
throwTo :: E.Exception e => ThreadId -> e -> MiniFu m ()
throwTo tid e = MiniFu (K.cont (\k -> ThrowTo tid e (k ())))

-- | Executes a computation with asynchronous exceptions masked. That
-- is, any thread which attempts to raise an exception in the current
-- thread with throwTo will be blocked until asynchronous exceptions
-- are unmasked again.
--
-- The argument passed to mask is a function that takes as its
-- argument another function, which can be used to restore the
-- prevailing masking state within the context of the masked
-- computation.  This function should not be used within an
-- 'uninterruptibleMask'.
mask :: ((forall x. MiniFu m x -> MiniFu m x) -> MiniFu m a) -> MiniFu m a
mask ma = MiniFu (K.cont (InMask E.MaskedInterruptible ma))

-- | Like mask, but the masked computation is not interruptible. THIS
-- SHOULD BE USED WITH GREAT CARE, because if a thread executing in
-- uninterruptibleMask blocks for any reason, then the thread (and
-- possibly the program, if this is the main thread) will be
-- unresponsive and unkillable. This function should only be necessary
-- if you need to mask exceptions around an interruptible operation,
-- and you can guarantee that the interruptible operation will only
-- block for a short period of time.  The supplied unmasking function
-- should not be used within a 'mask'.
uninterruptibleMask :: ((forall x. MiniFu m x -> MiniFu m x) -> MiniFu m a) -> MiniFu m a
uninterruptibleMask ma = MiniFu (K.cont (InMask E.MaskedUninterruptible ma))

-------------------------------------------------------------------------------

-- | Execute a concurrent computation with a given scheduler, and
-- return the result.
minifu :: C.MonadConc m => Scheduler s -> s -> MiniFu m a -> m (Maybe a, s)
minifu sched s (MiniFu ma) = do
  out <- C.newCRef Nothing
  s'  <- run sched s (K.runCont ma (Stop . C.writeCRef out . Just))
  a   <- C.readCRef out
  pure (a, s')

-------------------------------------------------------------------------------

-- | A simple random scheduler.
randomSched :: R.RandomGen g => Scheduler g
randomSched (t:|ts) g =
  let (i, g') = R.randomR (0, length ts) g
  in ((t:ts) !! i, g')
