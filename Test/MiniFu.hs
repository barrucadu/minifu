{-# LANGUAGE ScopedTypeVariables #-}
module Test.MiniFu
  ( module Test.MiniFu
  -- * Re-exports
  , MiniFu, ThreadId, MVar, CRef, Scheduler
  ) where

import qualified Control.Concurrent.Classy as C
import qualified Control.Exception as E
import Control.Monad (join)
import qualified Control.Monad.Cont as K
import Data.List.NonEmpty (NonEmpty(..))
import qualified System.Random as R

import Test.MiniFu.Internal

example :: MiniFu m Int
example = do
  a <- newEmptyMVar
  fork (putMVar a 1)
  fork (putMVar a 2)
  takeMVar a

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

example_async :: MiniFu m String
example_async = do
  a <- newEmptyMVar
  tid <- fork (putMVar a "hello from the other thread")
  throwTo tid E.ThreadKilled
  readMVar a

demo :: IO ()
demo = do
  g <- R.newStdGen
  print . fst =<< minifu randomSched g example

demo_sync :: IO ()
demo_sync = do
  g <- R.newStdGen
  print . fst =<< minifu randomSched g example_sync

demo_async :: IO ()
demo_async = do
  g <- R.newStdGen
  print . fst =<< minifu randomSched g example_async

-------------------------------------------------------------------------------

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

-- | Block until a value is present in the @MVar@, and then return
-- it. This does not "remove" the value, multiple reads are possible.
readMVar :: MVar m a -> MiniFu m a
readMVar v = MiniFu (K.cont (ReadMVar v))

-- | Take a value from a @MVar@. This "empties" the @MVar@, allowing a
-- new value to be put in. This will block if there is no value in the
-- @MVar@ already, until one has been put.
takeMVar :: MVar m a -> MiniFu m a
takeMVar v = MiniFu (K.cont (TakeMVar v))

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
