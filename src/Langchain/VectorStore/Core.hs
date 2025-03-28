module Langchain.VectorStore.Core 
  (VectorStore (..))
  where

import Langchain.DocumentLoader.Core
import Data.Text (Text)

--TODO: Add delete document mechanism, for this we need to generate and use id (Int)
class VectorStore m where
  
  addDocuments :: m -> [Document] -> IO (Either String m)
  -- delete :: m -> [Int] -> IO (Either String m)
  similaritySearch :: m -> Text -> Int -> IO (Either String [Document])
  similaritySearchByVector :: m -> [Float] -> Int -> IO (Either String [Document])
