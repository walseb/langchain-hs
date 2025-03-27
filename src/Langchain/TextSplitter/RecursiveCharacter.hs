{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Langchain.TextSplitter.RecursiveCharacter
  ( splitText
  , RecursiveCharacterOptions (..)
  , defaultRecursiveCharacterOptions 
  )
where

import Data.Text (Text)
import qualified Data.Text as T

-- Options for the Recursive Character Splitter
data RecursiveCharacterOptions = RecursiveCharacterOptions
  { separators :: [Text]
  , chunkSize :: Int
  , chunkOverlap :: Int
  , keepSeparator :: Bool
  }

-- Default options
defaultRecursiveCharacterOptions :: RecursiveCharacterOptions
defaultRecursiveCharacterOptions =
  RecursiveCharacterOptions
    { separators = ["\n\n", "\n", " ", ""]
    , chunkSize = 50
    , chunkOverlap = 20
    , keepSeparator = False
    }

-- Recursive character splitter function
splitText :: RecursiveCharacterOptions -> Text -> [Text]
splitText RecursiveCharacterOptions {..} txt =
  foldr (\sep acc -> mconcat $ map (T.splitOn sep) acc) [txt] separators
