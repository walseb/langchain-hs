{- |
Module      : Langchain.VectorStore.Core
Description : Core vector store abstraction for semantic search
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Haskell implementation of LangChain's vector store interface, providing:

- Document storage with vector embeddings
- Similarity-based search capabilities
- Integration with Runnable workflows

Example usage with hypothetical FAISS store:

@
-- Create vector store instance
faissStore :: FAISSStore
faissStore = emptyFAISSStore

-- Add documents with embeddings
docs = [Document "Haskell is functional" mempty, ...]
updatedStore <- addDocuments faissStore docs

-- Perform similarity search
results <- similaritySearch updatedStore "functional programming" 5
-- Returns top 5 relevant documents
@
-}
module Langchain.VectorStore.Core (VectorStore (..))
where

import Data.Int (Int64)
import Data.Text (Text)
import Langchain.DocumentLoader.Core

-- TODO: Add delete document mechanism, for this we need to generate and use id (Int)

{- | Vector store abstraction following LangChain's design patterns
Implementations should handle document storage, vectorization, and similarity search.

Example instance for an in-memory store:

@
data InMemoryStore = InMemoryStore
  { documents :: [Document]
  , embeddings :: [[Float]]
  }

instance VectorStore InMemoryStore where
  addDocuments store docs = ...
  similaritySearch store query k = ...
@
-}
class VectorStore m where
  -- | Add documents to the vector store
  --
  --   Example:
  --
  --   >>> addDocuments myStore [Document "Test content" mempty]
  --   Right (updatedStoreWithNewDocs)
  addDocuments :: m -> [Document] -> IO (Either String m)

  -- |
  --   Requires document ID tracking to be implemented in store instances.
  --
  --   Example usage (when implemented):
  --
  --   >>> delete myStore [123]
  --   Right (storeWithoutDoc123)
  delete :: m -> [Int64] -> IO (Either String m)

  -- | Find documents similar to query text
  --   Uses embedded vector representations for semantic search.
  --
  --   Example:
  --
  --   >>> similaritySearch store "Haskell monads" 3
  --   Right [Document "Monads in FP...", ...]
  similaritySearch :: m -> Text -> Int -> IO (Either String [Document])

  -- | Find documents similar to vector representation
  --   For direct vector comparisons without text conversion.
  --
  --   Example:
  --
  --   >>> similaritySearchByVector store [0.1, 0.3, ...] 5
  --   Right [mostSimilarDoc1, ...]
  similaritySearchByVector :: m -> [Float] -> Int -> IO (Either String [Document])

{- $examples
Test case patterns:
1. Document addition
   >>> addDocuments emptyStore [doc1, doc2]
   Right (storeWithDocs)

2. Similarity search
   >>> similaritySearch populatedStore "AI" 3
   Right [relevantDoc1, relevantDoc2, relevantDoc3]

3. Vector-based search
   >>> similaritySearchByVector store [0.5, 0.2, ...] 5
   Right [top5MatchingDocs]
-}
