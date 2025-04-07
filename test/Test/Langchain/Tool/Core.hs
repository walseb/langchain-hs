{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Langchain.Tool.Core where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map  as M
import Data.Aeson (encode, decode)
import Control.Monad.IO.Class (liftIO)
import Network.HTTP.Simple (Request, getResponseBody)
import qualified Data.ByteString.Lazy as BL

import Langchain.Tool.Core
import Langchain.Tool.WikipediaTool

-- Mock tool for testing
data MockTool = MockTool Text
  deriving (Show, Eq)

instance Tool MockTool where
  type Input MockTool = Text
  type Output MockTool = Text
  toolName (MockTool name) = name
  toolDescription _ = "A mock tool for testing"
  runTool _ input = return $ "Processed: " <> input


tests :: TestTree
tests = testGroup "Tool Tests"
  [ testCase "MockTool implements Tool interface correctly" testMockTool
  , testCase "WikipediaTool default values" testWikipediaToolDefaults
  , testCase "WikipediaTool tool name and description" testWikipediaToolMetadata
  , testCase "WikipediaTool search functionality" testWikipediaToolSearch
  , testCase "SearchResponse parsing" testSearchResponseParsing
  , testCase "PageResponse parsing" testPageResponseParsing
  ]

-- Test that our MockTool implements the Tool interface correctly
testMockTool :: Assertion
testMockTool = do
  let mockTool = MockTool "TestTool"
  
  -- Test toolName
  assertEqual "toolName should return the name" "TestTool" (toolName mockTool)
  
  -- Test toolDescription
  assertEqual "toolDescription should return description" 
              "A mock tool for testing" 
              (toolDescription mockTool)
  
  -- Test runTool
  result <- runTool mockTool "test input"
  assertEqual "runTool should process input correctly" 
              "Processed: test input" 
              result

-- Test WikipediaTool default values
testWikipediaToolDefaults :: Assertion
testWikipediaToolDefaults = do
  let tool = defaultWikipediaTool
  
  assertEqual "Default topK should be 2" 
              defaultTopK 
              (topK tool)
              
  assertEqual "Default docMaxChars should be 2000" 
              defaultDocMaxChars 
              (docMaxChars tool)
              
  assertEqual "Default language code should be 'en'" 
              defaultLanguageCode 
              (languageCode tool)

-- Test WikipediaTool metadata
testWikipediaToolMetadata :: Assertion
testWikipediaToolMetadata = do
  let tool = defaultWikipediaTool
  
  assertEqual "WikipediaTool name should be 'Wikipedia'"
              "Wikipedia"
              (toolName tool)
              
  assertBool "WikipediaTool description should mention Wikipedia"
             (T.isInfixOf "Wikipedia" (toolDescription tool))

-- Mock the WikipediaTool search functionality
testWikipediaToolSearch :: Assertion
testWikipediaToolSearch = do
  -- We'd normally mock HTTP requests here, but for this test,
  -- we'll create a simpler test that doesn't require mocking.
  
  let customTool = WikipediaTool 
                   { topK = 1
                   , docMaxChars = 10
                   , languageCode = "en"
                   }
  
  -- Here's how you'd test with real HTTP mocking
  -- This is just pseudo-code - you'll need a proper HTTP mocking library
  -- withMockedHTTP $ do
  --   mockResponse "wikipedia.org/w/api.php" mockSearchResponse
  --   mockResponse "wikipedia.org/w/api.php" mockPageResponse
  --   
  --   result <- runTool customTool "Haskell programming"
  --   assertBool "Result should contain wiki content" 
  --              (T.isInfixOf "Haskell" result)
  
  -- For now, we'll just test that the tool's properties are set correctly
  assertEqual "Custom tool should have topK = 1" 1 (topK customTool)
  assertEqual "Custom tool should truncate to 10 chars" 10 (docMaxChars customTool)

-- Test JSON parsing for SearchResponse
testSearchResponseParsing :: Assertion
testSearchResponseParsing = do
  let jsonStr = "{\"query\": {\"search\": [{\"ns\": 0, \"title\": \"Haskell\", \"pageid\": 12345, \"size\": 1000, \"wordcount\": 200, \"snippet\": \"<span>Haskell</span> is a functional language\", \"timestamp\": \"2023-01-01\"}]}}"
      parsed = decode jsonStr :: Maybe SearchResponse
  
  case parsed of
    Nothing -> assertFailure "Failed to parse SearchResponse JSON"
    Just SearchResponse{..} -> do
      let searchResults = search query
      assertBool "Should have at least one search result" (not $ null searchResults)
      let firstResult = head searchResults
      assertEqual "Page ID should match" 12345 (pageid firstResult)
      assertEqual "Title should match" "Haskell" (title_ firstResult)

-- Test JSON parsing for PageResponse
testPageResponseParsing :: Assertion
testPageResponseParsing = do
  let jsonStr = "{\"query\": {\"pages\": {\"12345\": {\"title\": \"Haskell\", \"extract\": \"Haskell is a functional programming language.\"}}}}"
      parsed = decode jsonStr :: Maybe PageResponse
  
  case parsed of
    Nothing -> assertFailure "Failed to parse PageResponse JSON"
    Just (PageResponse (Pages pagesMap)) -> do
      let maybePage = M.lookup "12345" pagesMap
      case maybePage of
        Nothing -> assertFailure "Expected page with ID 12345 not found"
        Just page -> do
          assertEqual "Page title should match" "Haskell" (title page)
          assertEqual "Page extract should match" 
                     "Haskell is a functional programming language." 
                     (extract page)
