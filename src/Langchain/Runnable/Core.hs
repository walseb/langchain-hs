{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{- |
Module      : Langchain.Runnable.Core
Description : Core Interface of Runnable. Necessary for LangChain Expression Language (LCEL)
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com

This module provides the core functionality for Runnable Interface in LangChain.
-}
module Langchain.Runnable.Core
  ( Runnable (..)
  ) where

-- | Core `Runnable` typeclass represents anything that can "run" with input and produce an output
class Runnable r where
  -- | The type input the runnable access
  type RunnableInput r

  -- | The type of output the runnable produces
  type RunnableOutput r

  -- | Core method to invoke (run) this component
  invoke :: r -> RunnableInput r -> IO (Either String (RunnableOutput r))

  -- | Batch process multiple inputs
  batch :: r -> [RunnableInput r] -> IO (Either String [RunnableOutput r])

  -- | Default implementation of batch
  batch r inputs = do
    results <- mapM (invoke r) inputs
    return $ sequence results

  -- | Stream results (for implementation that support streaming)
  stream :: r -> RunnableInput r -> (RunnableOutput r -> IO ()) -> IO (Either String ())

  -- | Default implementation that just calls invoke and then the callback
  stream r input callback = do
    result <- invoke r input
    case result of
      Left err -> return $ Left err
      Right output -> do
        callback output
        return $ Right ()
