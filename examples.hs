{-# LANGUAGE ScopedTypeVariables #-}

import qualified Control.Exception as E
import Control.Monad (join)
import qualified System.Random as R

import Test.MiniFu


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
