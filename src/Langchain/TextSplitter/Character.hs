{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Langchain.TextSplitter.Character
  ( splitText
  , CharacterSplitterOps (..)
  , defaultCharacterSplitterOps
  ) where

import Data.Text (Text)
import qualified Data.Text as T

data CharacterSplitterOps = CharacterSplitterOps
  { chunkSize :: Int
  , separator :: Text
  }
  deriving (Show, Eq)

defaultCharacterSplitterOps :: CharacterSplitterOps
defaultCharacterSplitterOps =
  CharacterSplitterOps
    { chunkSize = 100
    , separator = "\n\n"
    }

splitText :: CharacterSplitterOps -> Text -> [Text]
splitText CharacterSplitterOps {..} txt =
  mconcat $
    map
      (T.chunksOf chunkSize . T.strip)
      (if T.null separator then [txt] else T.splitOn separator txt )
