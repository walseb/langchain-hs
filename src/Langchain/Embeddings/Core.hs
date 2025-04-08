{- |
Module      : Langchain.Embeddings.Core
Description : Embedding model interface for LangChain Haskell
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Haskell implementation of LangChain's embedding model abstraction, providing:

- Document vectorization for semantic search
- Query embedding for similarity comparisons
- Integration with document loading pipelines

Example usage:

@
-- Hypothetical HuggingFace embedding instance
data HuggingFaceEmbeddings = HuggingFaceEmbeddings

instance Embeddings HuggingFaceEmbeddings where
  embedDocuments _ docs = do
    -- Convert documents to vectors using HuggingFace API
    return $ Right [[0.1, 0.3, ...], ...]

  embedQuery _ query = do
    -- Convert query to vector
    return $ Right [0.2, 0.4, ...]

-- Usage with loaded documents
docs <- load (FileLoader "data.txt")
case docs of
  Right documents -> do
    vectors <- embedDocuments HuggingFaceEmbeddings documents
    -- Use vectors for semantic search
  Left err -> print err
@
-}
module Langchain.Embeddings.Core
  ( -- * Embedding Interface
    Embeddings (..)
  ) where

import Data.Text (Text)
import Langchain.DocumentLoader.Core

{- | Typeclass for embedding models following LangChain's pattern.
Converts text/documents into numerical vectors for machine learning tasks.

Implementations should handle:

- Text preprocessing
- API calls to embedding services
- Error handling for failed requests
- Consistent vector dimensionality

Example instance for a test model:

@
data TestEmbeddings = TestEmbeddings

instance Embeddings TestEmbeddings where
  embedDocuments _ _ = return $ Right [[0.1, 0.2, 0.3]]
  embedQuery _ _ = return $ Right [0.4, 0.5, 0.6]
@
-}
class Embeddings m where
  -- | Convert documents to embedding vectors
  --
  --   Example:
  --
  --   >>> let doc = Document "Hello world" mempty
  --   >>> embedDocuments TestEmbeddings [doc]
  --   Right [[0.1, 0.2, 0.3]]
  embedDocuments :: m -> [Document] -> IO (Either String [[Float]])

  -- | Convert query text to embedding vector
  --
  --   Example:
  --
  --   >>> embedQuery TestEmbeddings "Search query"
  --   Right [0.4, 0.5, 0.6]
  embedQuery :: m -> Text -> IO (Either String [Float])

{- $examples
Test case patterns:

1. Document embedding
   >>> let docs = [Document "Test content" mempty]
   >>> embedDocuments TestEmbeddings docs
   Right [[0.1, 0.2, 0.3]]

2. Query embedding
   >>> embedQuery TestEmbeddings "Test query"
   Right [0.4, 0.5, 0.6]

3. Error handling
   >>> -- Simulate failed API call
   >>> embedQuery FaultyEmbeddings "Bad request"
   Left "API request failed"
-}
