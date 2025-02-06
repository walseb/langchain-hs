{-# LANGUAGE OverloadedStrings #-}

module Langchain.TextSplitter.RecursiveCharacter (
    recursiveCharacterSplitter,
    -- RecursiveCharacterOptions (..),
    -- defaultOptions,
)
where

import Data.Text (Text)
-- import qualified Data.Text as T

-- Options for the Recursive Character Splitter
data RecursiveCharacterOptions = RecursiveCharacterOptions
    { separators :: [Text]
    , chunkSize :: Int
    , chunkOverlap :: Int
    , keepSeparator :: Bool
    }

-- Default options
defaultOptions :: RecursiveCharacterOptions
defaultOptions =
    RecursiveCharacterOptions
        { separators = ["\n\n", "\n", " ", ""]
        , chunkSize = 50
        , chunkOverlap = 20
        , keepSeparator = False
        }

-- Recursive character splitter function
recursiveCharacterSplitter :: RecursiveCharacterOptions -> Text -> [Text]
recursiveCharacterSplitter opts text = splitText opts text (separators opts)

splitText :: RecursiveCharacterOptions -> Text -> [Text] -> [Text]
splitText = undefined
