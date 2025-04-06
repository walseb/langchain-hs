{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Langchain.OutputParser.Core
  ( OutputParser (..)
  , CommaSeparatedList (..)
  , JSONOutputStructure (..)
  , NumberSeparatedList (..)
  ) where

import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Internal.Search (indices)
import Data.Aeson
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString.Char8 (fromStrict)
import Data.Char (isDigit, isSpace)

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

-- | Represents a list of text items separated by numbers.
newtype NumberSeparatedList = NumberSeparatedList [Text]
  deriving (Show, Eq)

instance OutputParser NumberSeparatedList where
  parse txt =
    let s = trim (T.unpack txt)
    in case dropUntilAndConsumeBoundary s of
         Nothing   -> Left "No valid numbered items found"
         Just rest ->
           -- Parse the rest into items and wrap them in our newtype.
           Right . NumberSeparatedList . map (T.pack . trim) $ parseItems rest

-- | Drops noise until we find a valid boundary marker (number with dot)
-- and then consumes the marker.
dropUntilAndConsumeBoundary :: String -> Maybe String
dropUntilAndConsumeBoundary s =
  case findBoundary s of
    Nothing       -> Nothing
    Just (idx, n) -> Just (drop (idx + n) s)

-- | Scans the string for the next occurrence of a valid boundary.
-- Returns the index and length of the marker.
findBoundary :: String -> Maybe (Int, Int)
findBoundary s = go 0 s
  where
    go _ [] = Nothing
    go i xs@(_:rest) =
      case isBoundaryAt xs of
        Just n  -> Just (i, n)
        Nothing -> go (i + 1) rest

-- | Checks if the given string starts with a valid boundary.
-- A valid boundary has one or more digits, optional spaces,
-- a dot, then optional spaces. Returns the number of characters
-- consumed by the boundary if matched.
isBoundaryAt :: String -> Maybe Int
isBoundaryAt s =
  let (digits, rest1) = span isDigit s
  in if null digits
       then Nothing
       else
         let (spaces, rest2) = span isSpace rest1 in
         case rest2 of
           (c:rest3) | c == '.' ->
             let (spaces2, _) = span isSpace rest3
             in Just (length digits + length spaces + 1 + length spaces2)
           _ -> Nothing

-- | Recursively splits the string into items using the boundary markers.
parseItems :: String -> [String]
parseItems s =
  case findBoundary s of
    Nothing      -> [s]  -- No further boundaries; the rest is one item.
    Just (idx, n) ->
      let item = take idx s
          rest = drop (idx + n) s
      in item : parseItems rest

-- | A simple trim function to remove leading and trailing whitespace.
trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace
