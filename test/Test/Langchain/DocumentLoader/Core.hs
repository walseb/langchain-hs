{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.DocumentLoader.Core where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Text as T
import Data.Text (Text)
import Data.Map (Map, fromList, empty)
import qualified Data.Map as Map
import Data.Aeson (Value(..), Object, toJSON)
import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Control.Exception (bracket)
import Control.Monad (void)

import Langchain.DocumentLoader.Core
import Langchain.DocumentLoader.FileLoader
import Langchain.TextSplitter.Character

-- Helper functions for creating test files
createTestFile :: FilePath -> String -> IO ()
createTestFile path content = writeFile path content

withTestFile :: String -> (FilePath -> IO a) -> IO a
withTestFile content action = 
  withSystemTempDirectory "test-doc-loader" $ \dir -> do
    let filePath = dir </> "test-file.txt"
    createTestFile filePath content
    action filePath

-- Tests for Document type
documentTests :: TestTree
documentTests = testGroup "Document Tests"
  [ testCase "Document Semigroup instance should concatenate content and metadata" $ do
      let doc1 = Document "Hello" (fromList [("source", String "file1")])
          doc2 = Document " World" (fromList [("page", Number 1)])
          combined = doc1 <> doc2
      pageContent combined @?= "Hello World"
      metadata combined @?= fromList [("source", String "file1"), ("page", Number 1)]
      
  , testCase "Document Monoid instance should have identity element" $ do
      let doc = Document "Content" (fromList [("key", String "value")])
      doc <> mempty @?= doc
      mempty <> doc @?= doc
      pageContent mempty @?= ""
      metadata mempty @?= empty
  ]

-- Tests for FileLoader implementation
fileLoaderTests :: TestTree
fileLoaderTests = testGroup "FileLoader Tests"
  [ testCase "load should return document with file content and metadata" $ 
      withTestFile "Test content for the file." $ \filePath -> do
        result <- load (FileLoader filePath)
        case result of
          Left err -> assertFailure $ "Expected Right but got Left: " ++ err
          Right docs -> do
            length docs @?= 1
            let doc = head docs
            pageContent doc @?= "Test content for the file."
            Map.lookup "source" (metadata doc) @?= Just (String $ T.pack filePath)
      
  , testCase "load should return error for non-existent file" $ do
      result <- load (FileLoader "non-existent-file.txt")
      case result of
        Left err -> assertBool "Error message should mention file not found" 
                     (T.isInfixOf "File not found" (T.pack err))
        Right _ -> assertFailure "Expected Left for non-existent file but got Right"
      
  , testCase "loadAndSplit should split content using defaultCharacterSplitterOps" $ 
      withTestFile "Paragraph 1\n\nParagraph 2\n\nParagraph 3" $ \filePath -> do
        result <- loadAndSplit (FileLoader filePath)
        case result of
          Left err -> assertFailure $ "Expected Right but got Left: " ++ err
          Right chunks -> do
            chunks @?= ["Paragraph 1", "Paragraph 2", "Paragraph 3"]
      
  , testCase "loadAndSplit should return error for non-existent file" $ do
      result <- loadAndSplit (FileLoader "non-existent-file.txt")
      case result of
        Left err -> assertBool "Error message should mention file not found" 
                     (T.isInfixOf "File not found" (T.pack err))
        Right _ -> assertFailure "Expected Left for non-existent file but got Right"
      
  , testCase "load should handle empty files" $ 
      withTestFile "" $ \filePath -> do
        result <- load (FileLoader filePath)
        case result of
          Left err -> assertFailure $ "Expected Right but got Left: " ++ err
          Right docs -> do
            length docs @?= 1
            pageContent (head docs) @?= ""
      
  , testCase "load should handle large files" $ 
      withTestFile (concat $ replicate 1000 "Line of test content\n") $ \filePath -> do
        result <- load (FileLoader filePath)
        case result of
          Left err -> assertFailure $ "Expected Right but got Left: " ++ err
          Right docs -> do
            length docs @?= 1
            T.length (pageContent (head docs)) @?= 21000  -- 21 chars * 1000
  ]

-- Main test group
tests :: TestTree
tests = testGroup "Langchain.DocumentLoader Tests"
  [ documentTests
  , fileLoaderTests
  ]