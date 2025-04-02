module Langchain.Retriver.Core (
    Retriver (..)
  , VectorStoreRetriver (..)
    ) where

import Langchain.DocumentLoader.Core (Document)
import Langchain.VectorStore.Core

import Data.Text (Text)

class Retriver a where
  _get_relevant_documents :: a -> Text -> IO (Either String [Document])

newtype VectorStore a => VectorStoreRetriver a = VectorStoreRetriver { vs :: a }
  deriving (Eq, Show)

instance VectorStore a => Retriver (VectorStoreRetriver a) where
  _get_relevant_documents (VectorStoreRetriver v) query = similaritySearch v query 5
