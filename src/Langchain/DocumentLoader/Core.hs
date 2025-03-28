{- |
    Module for exposing BaseLoader Interface and Document type for DocumentLoader.
    https://python.langchain.com/docs/concepts/document_loaders/
    https://python.langchain.com/api_reference/core/document_loaders/langchain_core.document_loaders.base.BaseLoader.html
|
-}
module Langchain.DocumentLoader.Core
  ( Document (..)
  , BaseLoader (..)
  ) where

import Data.Aeson
import Data.Map (Map, empty)
import Data.Text (Text)

-- | Represents a loaded document with content and metadata.
data Document = Document
  { pageContent :: Text
  -- ^ The text content of the document
  , metadata :: Map Text Value
  -- ^ Additional metadata (e.g., source, page number)
  }
  deriving (Show, Eq)

instance Semigroup Document where
  doc1 <> doc2 = Document 
                    (pageContent doc1 <> pageContent doc2) 
                        (metadata doc1 <> metadata doc2)

instance Monoid Document where
  mempty = Document mempty empty

-- | Typeclass for document loaders, providing methods to load documents from various sources.
class BaseLoader m where
  -- | Load all documents from the source.
  load :: m -> IO (Either String [Document])

  -- | Load all the document and split them using recursiveCharacterSpliter
  loadAndSplit :: m -> IO (Either String [Text])

--  TODO: Implement lazy versions of Document and load.
-- Lazily load documents from the source.
-- lazyLoad :: m -> IO (Either String [Document])
