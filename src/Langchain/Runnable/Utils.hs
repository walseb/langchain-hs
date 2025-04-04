{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

-- This module is pretty experimental at the moment
module Langchain.Runnable.Utils (
    WithConfig(..)
  , Cached(..)
  , cached
  , Retry (..)
  , WithTimeout (..)
 ) where

import Langchain.Runnable.Core
import Control.Concurrent
import Data.Map.Strict as Map

-- | Runnables with configurable behavior
data WithConfig config r = (Runnable r) => 
  WithConfig 
    { configuredRunnable :: r
    , runnableConfig :: config 
    }

-- | Make WithConfig a Runnable that applies the configuration
instance (Runnable r) => Runnable (WithConfig config r) where
  type RunnableInput (WithConfig config r) = RunnableInput r
  type RunnableOutput (WithConfig config r) = RunnableOutput r
  
  invoke (WithConfig r1 _) input = invoke r1 input

-- | Cache results of a Runnable
data Cached r = (Runnable r, Ord (RunnableInput r)) =>
  Cached 
    { cachedRunnable :: r
    , cacheMap :: MVar (Map.Map (RunnableInput r) (RunnableOutput r))
    }

-- | Create a new cached runnable
cached :: (Runnable r, Ord (RunnableInput r)) => r -> IO (Cached r)
cached r = do
  cache <- newMVar Map.empty
  return $ Cached r cache

-- | Make Cached a Runnable that uses a cache
instance (Runnable r, Ord (RunnableInput r)) => Runnable (Cached r) where
  type RunnableInput (Cached r) = RunnableInput r
  type RunnableOutput (Cached r) = RunnableOutput r
  
  invoke (Cached r cacheRef) input = do
    cache <- readMVar cacheRef
    case Map.lookup input cache of
      Just output -> return $ Right output
      Nothing -> do
        result <- invoke r input
        case result of
          Left err -> return $ Left err
          Right output -> do
            modifyMVar_ cacheRef $ \c -> return $ Map.insert input output c
            return $ Right output

-- | Add retry capability to any Runnable
data Retry r = (Runnable r) =>
  Retry
    { retryRunnable :: r
    , maxRetries :: Int
    , retryDelay :: Int  -- microseconds
    }

-- | Make Retry a Runnable that retries on failure
instance (Runnable r) => Runnable (Retry r) where
  type RunnableInput (Retry r) = RunnableInput r
  type RunnableOutput (Retry r) = RunnableOutput r
  
  invoke (Retry r maxRetries_ delay) input = retryWithCount 0
    where
      retryWithCount count = do
        result <- invoke r input
        case result of
          Left err -> 
            if count < maxRetries_
              then do
                threadDelay delay
                retryWithCount (count + 1)
              else return $ Left err
          Right output -> return $ Right output

-- | Add timeouts to any Runnable
data WithTimeout r = (Runnable r) =>
  WithTimeout
    { timeoutRunnable :: r
    , timeoutMicroseconds :: Int
    }

-- | Make WithTimeout a Runnable that times out
instance (Runnable r) => Runnable (WithTimeout r) where
  type RunnableInput (WithTimeout r) = RunnableInput r
  type RunnableOutput (WithTimeout r) = RunnableOutput r
  
  invoke (WithTimeout r timeout) input = do
    resultVar <- newEmptyMVar
    
    -- Fork a thread to run the computation
    tid <- forkIO $ do
      result <- invoke r input
      putMVar resultVar (Just result)
    
    -- Set up the timeout
    timeoutTid <- forkIO $ do
      threadDelay timeout
      putMVar resultVar Nothing
    
    -- Wait for either result or timeout
    result <- takeMVar resultVar
    
    -- Kill the other thread
    killThread tid
    killThread timeoutTid
    
    case result of
      Just r_ -> return r_
      Nothing -> return $ Left "Operation timed out"
