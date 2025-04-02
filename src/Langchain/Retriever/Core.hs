module Langchain.Retriever.Core (
    Retriever (..)
  , VectorStoreRetriever (..)
    ) where

import Langchain.DocumentLoader.Core (Document)
import Langchain.VectorStore.Core

import Data.Text (Text)

class Retriever a where
  _get_relevant_documents :: a -> Text -> IO (Either String [Document])

newtype VectorStore a => VectorStoreRetriever a = VectorStoreRetriever { vs :: a }
  deriving (Eq, Show)

instance VectorStore a => Retriever (VectorStoreRetriever a) where
  _get_relevant_documents (VectorStoreRetriever v) query = similaritySearch v query 5
