{-# LANGUAGE RecordWildCards #-}

module Langchain.Embeddings.Ollama
  ( OllamaEmbeddings (..)
  ) where

import Data.Maybe
import Data.Ollama.Embeddings
import Data.Text (Text)
import Langchain.DocumentLoader.Core
import Langchain.Embeddings.Core

-- | Configuration for the Ollama embeddings model.
data OllamaEmbeddings = OllamaEmbeddings
  { model :: Text
  -- ^ The name of the Ollama model to use for embeddings
  , defaultTruncate :: Maybe Bool
  -- ^ Optional flag to truncate input if supported by the API
  , defaultKeepAlive :: Maybe Text
  -- ^ Optional parameter to keep the model alive
  }
  deriving (Show, Eq)

go :: EmbeddingResp -> Either String [Float]
go embResp =
  case listToMaybe (embedding_ embResp) of
    Nothing -> Left "Embeddings are empty"
    Just x -> Right x

-- | Implementation of the 'Embeddings' typeclass for 'OllamaEmbeddings'.
instance Embeddings OllamaEmbeddings where
  -- \| Embed multiple Documebts by adapting the single-text 'embedding' function.
  embedDocuments (OllamaEmbeddings {..}) docs = do
    -- For each input text, make an individual API call
    results <- mapM (\doc -> embeddingOps model (pageContent doc) defaultTruncate defaultKeepAlive) docs
    -- Combine the results, handling errors appropriately
    return $
      sequence results >>= \resps ->
        mapM go resps

  -- \| Embed a single query text using the 'embedding' function.
  embedQuery (OllamaEmbeddings {..}) query = do
    res <- embeddingOps model query defaultTruncate defaultKeepAlive
    case fmap embedding_ res of
      Left err -> pure $ Left err
      Right lst ->
        case listToMaybe lst of
          Nothing -> pure $ Left "Embeddings are empty"
          Just x -> pure $ Right x
