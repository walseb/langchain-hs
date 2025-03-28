{-
https://python.langchain.com/docs/concepts/embedding_models/
-}
module Langchain.Embeddings.Core
  ( Embeddings (..)
  ) where

import Data.Text (Text)
import Langchain.DocumentLoader.Core

class Embeddings m where
  embedDocuments :: m -> [Document] -> IO (Either String [[Float]])
  embedQuery :: m -> Text -> IO (Either String [Float])
