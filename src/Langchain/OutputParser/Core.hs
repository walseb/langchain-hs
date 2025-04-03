{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Langchain.OutputParser.Core
  ( OutputParser (..)
  , CommaSeparatedList (..)
  ) where

import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Internal.Search (indices)
import Data.Aeson
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString.Char8 (fromStrict)

-- | Typeclass for parsing output from language models into specific types.
class OutputParser a where
  parse :: Text -> Either String a

-- | Represents a list of text items separated by commas.
newtype CommaSeparatedList = CommaSeparatedList [Text]
  deriving (Show, Eq)

instance OutputParser Bool where
  parse txt = do
     let txt' = T.strip $ T.toLower txt
     if length (indices "true" txt') > 0 then 
        Right True
     else if length (indices "false" txt') > 0 then
        Right False
     else
        Left "Invalid boolean value"

instance OutputParser CommaSeparatedList where
  parse txt = Right $ CommaSeparatedList $ map T.strip $ T.splitOn "," txt

newtype FromJSON a => JSONOutputStructure a = JSONOutputStructure
  { jsonValue :: a
  } deriving (Show, Eq, FromJSON)

-- | Instance for parsing JSON into any type that implements FromJSON.
instance FromJSON a => OutputParser (JSONOutputStructure a) where
  parse txt =
    case eitherDecode (fromStrict $ encodeUtf8 txt) of
      Left err -> Left $ "JSON parsing error: " ++ err
      Right val -> Right val
