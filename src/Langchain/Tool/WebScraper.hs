{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : Langchain.Tool.WebScraper
Description : Tool for scrapping text content from URL
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental
-}
module Langchain.Tool.WebScraper (WebScraper (..), WebPageInfo (..), fetchAndScrape) where

import Control.Exception (SomeException, try)
import Data.Aeson (ToJSON)
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import Langchain.Tool.Core
import Network.HTTP.Simple
import Text.HTML.Scalpel

-- | Represents a web scraper tool that extracts content from web pages
data WebScraper = WebScraper
  deriving (Show)

-- | Stores the extracted webpage information
data WebPageInfo = WebPageInfo
  { pageTitle :: Maybe Text
  , pageHeadings :: [Text]
  , pageLinks :: [(Text, Text)] -- (Link text, URL)
  , pageText :: Text
  }
  deriving (Show, Generic)

-- Make WebPageInfo serializable to JSON
instance ToJSON WebPageInfo

-- | Input type for the WebScraper - just a URL
type ScraperInput = Text

-- | Implement the Tool typeclass for WebScraper
instance Tool WebScraper where
  type Input WebScraper = ScraperInput
  type Output WebScraper = Text

  toolName _ = "web_scraper"

  toolDescription _ =
    "Scrapes content from a webpage. Provide a valid URL, and it will extract the title,"
      <> "headings, links, and text content."

  runTool _ url = do
    result <- fetchAndScrape url
    case result of
      Left err -> pure $ "Error scraping webpage: " <> T.pack (show err)
      Right info -> pure $ T.pack (show info)

-- | Fetch HTML content from a URL and extract webpage information
fetchAndScrape :: Text -> IO (Either String WebPageInfo)
fetchAndScrape url = do
  request_ <- parseRequest (T.unpack url)
  eResp <- try $ httpLBS request_ :: IO (Either SomeException (Response LBS.ByteString))
  case eResp of
    Left err -> pure $ Left (show err)
    Right r -> do
      let rBody = (getResponseBody r)
      let htmlContent = TE.decodeUtf8 $ LBS.toStrict rBody
      let scraped = scrapeStringLike htmlContent scrapeWebPageInfo
      case scraped of
        Nothing -> pure $ Left "Failed to parse HTML content"
        Just info -> pure $ Right info

-- | Define the Scalpel scraper for extracting webpage information
scrapeWebPageInfo :: Scraper Text WebPageInfo
scrapeWebPageInfo = do
  title <- scrapeTitle
  headings <- scrapeHeadings
  links <- scrapeLinks
  t <- scrapeText
  return $ WebPageInfo title headings links t

-- | Scrape the page title
scrapeTitle :: Scraper Text (Maybe Text)
scrapeTitle = fmap listToMaybe $ texts "title"

-- | Scrape all headings (h1-h6)
scrapeHeadings :: Scraper Text [Text]
scrapeHeadings = do
  h1s <- texts "h1"
  h2s <- texts "h2"
  h3s <- texts "h3"
  h4s <- texts "h4"
  h5s <- texts "h5"
  h6s <- texts "h6"
  return $ concat [h1s, h2s, h3s, h4s, h5s, h6s]

-- | Scrape all links with their URLs
scrapeLinks :: Scraper Text [(Text, Text)]
scrapeLinks = chroots "a" $ do
  linkText <- text "a"
  linkHref <- attr "href" "a"
  return (linkText, linkHref)

-- | Scrape main text content (from p, div, span elements)
scrapeText :: Scraper Text Text
scrapeText = do
  paragraphs <- texts "p"
  divs <- texts "div"
  spans <- texts "span"
  listElems <- texts "li"
  return $ T.intercalate "\n\n" $ filter (not . T.null) $ concat [paragraphs, divs, spans, listElems]
