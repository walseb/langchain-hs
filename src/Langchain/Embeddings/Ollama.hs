{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Langchain.Embeddings.Ollama
Description : Ollama integration for text embeddings in LangChain Haskell
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Ollama implementation of LangChain's embedding interface. Supports document and query
embedding generation through Ollama's API.

Example usage:

@
-- Create Ollama embeddings configuration
ollamaEmb = OllamaEmbeddings
  { model = "llama3"
  , defaultTruncate = Just True
  , defaultKeepAlive = Just "5m"
  }

-- Embed query text
queryVec <- embedQuery ollamaEmb "What is Haskell?"
-- Right [0.12, 0.34, ...]

-- Embed document collection
doc <- Document "Haskell is a functional programming language" mempty
docsVec <- embedDocuments ollamaEmb [doc]
-- Right [[0.56, 0.78, ...]]
@
-}
module Langchain.Embeddings.Ollama
  ( OllamaEmbeddings (..)
  ) where

import Data.Maybe
import Data.Ollama.Embeddings
import Data.Text (Text)
import Langchain.DocumentLoader.Core
import Langchain.Embeddings.Core

{- | Ollama-specific embedding configuration
Contains parameters for controlling:

- Model selection
- Input truncation behavior
- Model caching via keep-alive

Example configuration:

>>> OllamaEmbeddings "nomic-embed" (Just False) (Just "1h")
OllamaEmbeddings {model = "nomic-embed", ...}
-}
data OllamaEmbeddings = OllamaEmbeddings
  { model :: Text
  -- ^ The name of the Ollama model to use for embeddings
  , defaultTruncate :: Maybe Bool
  -- ^ Optional flag to truncate input if supported by the API
  , defaultKeepAlive :: Maybe Text
  -- ^ Keep model loaded for specified duration (e.g., "5m")
  }
  deriving (Show, Eq)

go :: EmbeddingResp -> Either String [Float]
go embResp =
  case listToMaybe (embedding_ embResp) of
    Nothing -> Left "Embeddings are empty"
    Just x -> Right x

{- | Ollama implementation of the 'Embeddings' interface [[6]].
Uses Ollama's embedding API for vector generation. Handles:
- Multiple document embedding via batch processing
- Query embedding for similarity searches
- Error propagation from API responses

Example instance usage:
@
-- Embed multiple documents
docs <- load (FileLoader "data.txt")
case docs of
  Right documents -> do
    vecs <- embedDocuments ollamaEmb documents
    -- Use vectors for semantic search
  Left err -> print err
@
-}
instance Embeddings OllamaEmbeddings where
  -- \| Document embedding implementation [[3]]:
  --  Processes each document individually through Ollama's API.
  --
  --  Example:
  --  >>> let doc = Document "Test content" mempty
  --  >>> embedDocuments ollamaEmb [doc]
  --  Right [[0.1, 0.2, ...], ...]
  --
  embedDocuments (OllamaEmbeddings {..}) docs = do
    -- For each input text, make an individual API call
    results <- mapM (\doc -> embeddingOps model (pageContent doc) defaultTruncate defaultKeepAlive) docs
    -- Combine the results, handling errors appropriately
    return $
      sequence results >>= \resps ->
        mapM go resps

  -- \| Query embedding implementation:
  --  Generates vector representation for search queries.
  --
  --  Example:
  --  >>> embedQuery ollamaEmb "Explain monads"
  --  Right [0.3, 0.4, ...]
  --
  embedQuery (OllamaEmbeddings {..}) query = do
    res <- embeddingOps model query defaultTruncate defaultKeepAlive
    case fmap embedding_ res of
      Left err -> pure $ Left err
      Right lst ->
        case listToMaybe lst of
          Nothing -> pure $ Left "Embeddings are empty"
          Just x -> pure $ Right x
