{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Langchain.Tool.WikipediaTool
  ( defaultTopK
  , WikipediaTool (..)
  , defaultDocMaxChars
  , defaultLanguageCode
  , defaultWikipediaTool
  , SearchQuery (..)
  , SearchResponse (..)
  , Page (..)
  , SearchResult (..)
  , Pages (..)
  , PageResponse (..)
  ) where

import Control.Exception (throwIO)
import Data.Aeson (FromJSON (..), decode, withObject, (.:))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import Langchain.Tool.Core
import Network.HTTP.Simple

-- | Data type representing the Wikipedia tool.
data WikipediaTool = WikipediaTool
  { topK :: Int
  -- ^ Number of Wikipedia pages to include in the result.
  , docMaxChars :: Int
  -- ^ Number of characters to take from each page.
  , languageCode :: Text
  -- ^ Language code to use (e.g., "en" for English).
  }
  deriving (Eq, Show)

-- | Default values for the Wikipedia tool.
defaultTopK :: Int
defaultTopK = 2

defaultDocMaxChars :: Int
defaultDocMaxChars = 2000

defaultLanguageCode :: Text
defaultLanguageCode = "en"

-- | Create a new Wikipedia tool with default values.
defaultWikipediaTool :: WikipediaTool
defaultWikipediaTool =
  WikipediaTool
    { topK = defaultTopK
    , docMaxChars = defaultDocMaxChars
    , languageCode = defaultLanguageCode
    }

-- | Tool instance for WikipediaTool.
instance Tool WikipediaTool where
  type Input WikipediaTool = Text

  -- \^ Input is a search query.
  type Output WikipediaTool = Text

  -- \^ Output is the combined page extracts.
  toolName _ = "Wikipedia"

  toolDescription _ =
    "A wrapper around Wikipedia. Useful for answering general questions about people, places, companies, facts, historical events, or other subjects. Input should be a search query."

  runTool tool q = searchWiki tool q

-- | Perform a Wikipedia search and retrieve page extracts.
searchWiki :: WikipediaTool -> Text -> IO Text
searchWiki tool q = do
  SearchResponse {..} <- performSearch tool q
  if null (search query)
    then return "no wikipedia pages found"
    else do
      let pageIds = map pageid (take (topK tool) (search query))
      pages <- mapM (getPage tool) pageIds
      let extracts = map (T.take (docMaxChars tool) . extract) pages
      return $ T.intercalate "\n\n" extracts

-- | Perform a search on Wikipedia.
performSearch :: WikipediaTool -> Text -> IO SearchResponse
performSearch tool q = do
  let params =
        M.fromList
          [ ("format", "json")
          , ("action", "query")
          , ("list", "search")
          , ("srsearch", T.unpack q)
          , ("srlimit", show (topK tool))
          ]
      url =
        T.pack $
          "https://" <> T.unpack (languageCode tool) <> ".wikipedia.org/w/api.php?" <> urlEncode params
  request <- parseRequest (T.unpack url)
  response <- httpLbs request
  let body = getResponseBody response
  case decode body of
    Just result -> return result
    Nothing -> throwIO $ userError "Failed to decode search response"

-- | Get a page extract from Wikipedia.
getPage :: WikipediaTool -> Int -> IO Page
getPage tool pageId = do
  let params =
        M.fromList
          [ ("format", "json")
          , ("action", "query")
          , ("prop", "extracts")
          , ("pageids", show pageId)
          ]
      url =
        T.pack $
          "https://" <> T.unpack (languageCode tool) <> ".wikipedia.org/w/api.php?" <> urlEncode params
  request <- parseRequest (T.unpack url)
  response <- httpLbs request
  let body = getResponseBody response
  case decode body of
    Just (PageResponse (Pages p)) -> case M.lookup (show pageId) p of
      Just page -> return page
      Nothing -> throwIO $ userError "Page not found in response"
    Nothing -> throwIO $ userError "Failed to decode page response"

-- | URL encode a map of parameters.
urlEncode :: Map String String -> String
urlEncode = concatMap (\(k, v) -> k ++ "=" ++ v ++ "&") . M.toList

-- | Data types for JSON parsing.
data SearchResponse = SearchResponse
  { query :: SearchQuery
  }
  deriving (Show, Generic, FromJSON)

data SearchQuery = SearchQuery
  { search :: [SearchResult]
  }
  deriving (Show)

instance FromJSON SearchQuery where
  parseJSON = withObject "SearchQuery" $ \v ->
    SearchQuery
      <$> v .: "search"

data SearchResult = SearchResult
  { ns :: Int
  , title_ :: Text
  , pageid :: Int
  , size :: Int
  , wordcount :: Int
  , snippet :: Text
  , timestamp :: Text
  }
  deriving (Show)

instance FromJSON SearchResult where
  parseJSON = withObject "SearchResult" $ \v ->
    SearchResult
      <$> v .: "ns"
      <*> v .: "title"
      <*> v .: "pageid"
      <*> v .: "size"
      <*> v .: "wordcount"
      <*> v .: "snippet"
      <*> v .: "timestamp"

data PageResponse = PageResponse
  { query :: Pages
  }
  deriving (Generic, Eq, Show, FromJSON)

data Pages = Pages
  { pages :: Map String Page
  }
  deriving (Generic, Eq, Show, FromJSON)

data Page = Page
  { title :: Text
  , extract :: Text
  }
  deriving (Show, Eq)

instance FromJSON Page where
  parseJSON = withObject "Page" $ \v ->
    Page
      <$> v .: "title"
      <*> v .: "extract"
