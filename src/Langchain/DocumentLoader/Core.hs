{- |
    Module for exposing BaseLoader Interface and Document type for DocumentLoader. 
    https://python.langchain.com/docs/concepts/document_loaders/
    https://python.langchain.com/api_reference/core/document_loaders/langchain_core.document_loaders.base.BaseLoader.html
| -}
module Langchain.DocumentLoader.Core (
    Document (..)
  , BaseLoader (..)
  ) where

import Data.Text (Text)
import Data.Map (Map)
import Data.Aeson 

-- | Represents a loaded document with content and metadata.
data Document = Document
    { pageContent :: Text          -- ^ The text content of the document
    , metadata :: Map Text Value   -- ^ Additional metadata (e.g., source, page number)
    }
    deriving (Show, Eq)

-- | Typeclass for document loaders, providing methods to load documents from various sources.
class BaseLoader m where
    -- | Load all documents from the source.
    load :: m -> IO (Either String [Document])

    -- | Load all the document and split them using recursiveCharacterSpliter
    loadAndSplit :: m -> IO (Either String [Text])
    
    --  TODO: Implement lazy versions of Document and load.
    -- Lazily load documents from the source.
    -- lazyLoad :: m -> IO (Either String [Document])
