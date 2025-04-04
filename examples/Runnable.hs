module Main where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T

import Langchain.LLM.Core 
import Langchain.PromptTemplate (PromptTemplate(..))
import Langchain.Runnable.Core (Runnable(..))
import Langchain.Tool.Core (Tool(..))
import Langchain.Retriever.Core (Retriever(..))
import Langchain.OutputParser.Core (OutputParser(..))
import Langchain.DocumentLoader.Core
import        Langchain.OutputParser.Core
import        Langchain.PromptTemplate
import        Langchain.Retriever.Core
import        Langchain.Runnable.Core
import        Langchain.Tool.Core
import        Langchain.VectorStore.Core

-- | Simple example of chaining a prompt template to an LLM
promptToLLM :: PromptTemplate -> Ollama -> HM.Map Text Text -> IO (Either String Text)
promptToLLM template model variables = chain template model variables

-- | Example of a RAG (Retrieval Augmented Generation) pipeline
ragPipeline :: (Retriever r, LLM m) => r -> PromptTemplate -> m -> Text -> IO (Either String Text)
ragPipeline retriever template model query = do
  -- Retrieve documents based on query
  docsResult <- invoke retriever query
  case docsResult of
    Left err -> return $ Left err
    Right docs -> do
      -- Build context from documents
      let context = T.intercalate "\n\n" $ map pageContent docs
          variables = HM.fromList [("query", query), ("context", context)]
      
      -- Generate prompt from template and variables
      promptResult <- invoke template variables
      case promptResult of
        Left err -> return $ Left err
        Right prompt -> do
          -- Generate response from LLM
          invoke model prompt

-- | Example of using a tool with Runnable interface
useSearchTool :: (Tool t, Input t ~ Text, Output t ~ Text) => t -> Text -> IO (Either String Text)
useSearchTool tool query = invoke tool query

-- | Example of using an OutputParser
parseStructuredOutput :: (OutputParser a) => Text -> IO (Either String a)
parseStructuredOutput output = pure $ parse output

-- | Full example: Query -> Retriever -> PromptTemplate -> LLM -> OutputParser -> Result
fullPipeline :: 
  (Retriever r, LLM m, OutputParser p, RunnableOutput p ~ a) => 
  r -> PromptTemplate -> m -> p -> Text -> IO (Either String a)
fullPipeline retriever template model parser query = do
  -- Step 1: Retrieve documents
  docsResult <- invoke retriever query
  case docsResult of
    Left err -> return $ Left err
    Right docs -> do
      -- Step 2: Format prompt with context
      let context = T.intercalate "\n\n" $ map pageContent docs
          variables = HM.fromList [("query", query), ("context", context)]
      
      promptResult <- invoke template variables
      case promptResult of
        Left err -> return $ Left err
        Right prompt -> do
          -- Step 3: Generate response from LLM
          responseResult <- invoke model prompt
          case responseResult of
            Left err -> return $ Left err
            Right response -> do
              -- Step 4: Parse structured output
              pure $ parse response
