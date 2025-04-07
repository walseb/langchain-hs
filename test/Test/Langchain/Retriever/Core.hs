{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Retriever.Core (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Retriever.MultiQueryRetriever
import Langchain.Retriever.Core (Retriever(..))
import Langchain.DocumentLoader.Core (Document(..))
import Langchain.LLM.Core (LLM(..))
import Langchain.PromptTemplate (PromptTemplate(..))
import Langchain.OutputParser.Core (NumberSeparatedList(..))

import qualified Data.Map.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T

-- Dummy LLM that simulates generating query variations.
data DummyLLM = DummyLLM

instance LLM DummyLLM where
  -- When 'generate' is called, we return a fixed response in the format expected by the
  -- NumberSeparatedList parser. For example:
  --
  -- "1. test query 1\n2. test query 2"
  generate _ prompt _ = return $ Right "1. test query 1\n2. test query 2"
  chat _ _ _ = return $ Right "dummy chat response"
  stream _ _ _ _ = return $ Right ()

-- Dummy Retriever that simulates retrieving a document for a given query.
data DummyRetriever = DummyRetriever

instance Retriever DummyRetriever where
  _get_relevant_documents _ query = 
    -- Return a single document whose pageContent is the query appended with " result"
    return $ Right [Document (query <> " result") HM.empty]

--------------------------------------------------------------------------------
-- Test Cases
--------------------------------------------------------------------------------

-- Test the generateQueries function to ensure it returns the expected queries.
test_generateQueries :: Assertion
test_generateQueries = do
  let dummyLLM = DummyLLM
      query = "original query"
      numQueriesToGenerate = 2
      includeOriginal = True
      queryPrompt = defaultQueryGenerationPrompt
  result <- generateQueries dummyLLM queryPrompt query numQueriesToGenerate includeOriginal
  case result of
    Left err -> assertFailure ("generateQueries failed with error: " ++ err)
    Right qs -> do
      -- Expecting the original query to be added, plus the two generated queries.
      let expectedQueries = [ "original query"
                            , "test query 1"
                            , "test query 2"
                            ]
      length qs @?= 3
      qs @?= expectedQueries

-- Test the MultiQueryRetriever _get_relevant_documents implementation.
test_MultiQueryRetriever :: Assertion
test_MultiQueryRetriever = do
  let dummyLLM = DummyLLM
      dummyRetriever = DummyRetriever
      -- Create a MultiQueryRetriever using the dummy implementations.
      mqRetriever = newMultiQueryRetriever dummyRetriever dummyLLM
      originalQuery = "original query"
  result <- _get_relevant_documents mqRetriever originalQuery
  case result of
    Left err -> assertFailure ("MultiQueryRetriever failed with error: " ++ err)
    Right docs -> do
      -- Since generateQueries returns three queries (original plus two generated),
      -- and DummyRetriever returns one document per query, we expect 3 documents.
      length docs @?= 3
      let contents = map pageContent docs
          expectedContents = [ "original query result"
                             , "test query 1 result"
                             , "test query 2 result"
                             ]
      contents @?= expectedContents

--------------------------------------------------------------------------------
-- Main test runner
--------------------------------------------------------------------------------

tests = testGroup "Retriever Tests"
  [ testCase "generateQueries returns expected queries" test_generateQueries
  , testCase "MultiQueryRetriever retrieves and combines documents" test_MultiQueryRetriever
  ]