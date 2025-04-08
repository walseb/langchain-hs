{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{- |
Module      : Langchain.Runnable.Core
Description : Core Interface of Runnable. Necessary for LangChain Expression Language (LCEL)
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>

This module defines the 'Runnable' typeclass, which is the fundamental abstraction in the
Haskell implementation of LangChain Expression Language (LCEL). A 'Runnable' represents any
component that can process an input and produce an output, potentially with side effects.

The 'Runnable' abstraction enables composition of various LLM-related components into
processing pipelines, including:

* Language Models
* Prompt Templates
* Document Retrievers
* Text Splitters
* Embedders
* Vector Stores
* Output Parsers

By implementing the 'Runnable' typeclass, components can be combined using the combinators
provided in "Langchain.Runnable.Chain".
-}
module Langchain.Runnable.Core
  ( Runnable (..)
  ) where

{- | The core 'Runnable' typeclass represents anything that can "run" with an input and produce an output.

This typeclass is the foundation of the LangChain Expression Language (LCEL) in Haskell,
allowing different components to be composed into processing pipelines.

To implement a 'Runnable', you must:

1. Define the input and output types using associated type families
2. Implement the 'invoke' method
3. Optionally override 'batch' and 'stream' for specific optimizations

Example implementation:

@
data TextSplitter = TextSplitter { chunkSize :: Int, overlap :: Int }

instance Runnable TextSplitter where
  type RunnableInput TextSplitter = String
  type RunnableOutput TextSplitter = [String]

  invoke splitter text = do
    -- Implementation of text splitting logic
    let chunks = splitTextIntoChunks (chunkSize splitter) (overlap splitter) text
    return $ Right chunks
@
-}
class Runnable r where
  -- | The type of input the runnable accepts.
  --
  -- For example, an LLM might accept 'String' or 'PromptValue' as input.
  type RunnableInput r

  -- | The type of output the runnable produces.
  --
  -- For example, an LLM might produce 'String' or 'LLMResult' as output.
  type RunnableOutput r

  -- | Core method to invoke (run) this component with a single input.
  --
  --   This is the primary method that must be implemented for any 'Runnable'.
  --   It processes a single input and returns either an error message or the output.
  --
  --   Example usage:
  --
  --   @
  --   let model = OpenAI { temperature = 0.7, model = "gpt-3.5-turbo" }
  --   result <- invoke model "Explain monads in simple terms."
  --   case result of
  --     Left err -> putStrLn $ "Error: " ++ err
  --     Right response -> putStrLn response
  --   @
  invoke :: r -> RunnableInput r -> IO (Either String (RunnableOutput r))

  -- | Batch process multiple inputs at once.
  --
  --   This method can be overridden to provide more efficient batch processing,
  --   particularly for components like LLMs that may have batch APIs.
  --
  --   The default implementation simply maps 'invoke' over each input and
  --   sequences the results.
  --
  --   Example usage:
  --
  --   @
  --   let retriever = VectorDBRetriever { ... }
  --   questions <- ["What is Haskell?", "Explain monads.", "How do I install GHC?"]
  --   result <- batch retriever questions
  --   case result of
  --     Left err -> putStrLn $ "Batch processing failed: " ++ err
  --     Right docs -> mapM_ print docs
  --   @
  batch :: r -> [RunnableInput r] -> IO (Either String [RunnableOutput r])

  -- | Default implementation of batch that processes each input sequentially
  batch r inputs = do
    results <- mapM (invoke r) inputs
    return $ sequence results

  -- | Stream results for components that support streaming.
  --
  --   This method is particularly useful for LLMs that can stream tokens as they're
  --   generated, allowing for more responsive user interfaces.
  --
  --   The callback function is called with each piece of the output as it becomes available.
  --
  --   Example usage:
  --
  --   @
  --   let model = OpenAI { temperature = 0.7, model = "gpt-3.5-turbo", streaming = True }
  --   result <- stream model "Write a story about a programmer." $ \chunk -> do
  --     putStr chunk
  --     hFlush stdout
  --   case result of
  --     Left err -> putStrLn $ "\\nError: " ++ err
  --     Right _ -> putStrLn "\\nStreaming completed successfully."
  --   @
  stream :: r -> RunnableInput r -> (RunnableOutput r -> IO ()) -> IO (Either String ())

  -- | Default implementation that invokes the runnable and then calls the callback with the full result
  stream r input callback = do
    result <- invoke r input
    case result of
      Left err -> return $ Left err
      Right output -> do
        callback output
        return $ Right ()
