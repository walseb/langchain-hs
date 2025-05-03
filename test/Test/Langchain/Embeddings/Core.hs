{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Embeddings.Core (tests) where

import Data.Text (isInfixOf, pack)
import Langchain.DocumentLoader.Core
import Langchain.Embeddings.Core
import Langchain.Embeddings.Ollama
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  sequentialTestGroup
    "Embedding Tests"
    AllFinish
    [ testGroup
        "embedQuery Tests"
        [ testCase "Returns embedding on successful response" $ do
            let embeddings = OllamaEmbeddings "llama3.2:latest" Nothing Nothing
            result <- embedQuery embeddings "test query"
            case result of
              Left err -> assertFailure $ "Expected success, got error: " ++ err
              Right vec -> assertEqual "Correct embedding length" 3072 (length vec)
        , testCase "Propagates API errors" $ do
            let embeddings = OllamaEmbeddings "error-model" Nothing Nothing
            -- Assuming embeddingOps returns Left "API Failure"
            result <- embedQuery embeddings "error query"
            case result of
              Left err -> assertBool "Error message contains 'error'" ("error" `isInfixOf` (pack err))
              Right _ -> assertFailure "Expected API error propagation"
        ]
    , testGroup
        "embedDocuments Tests"
        [ testCase "Processes multiple documents successfully" $ do
            let embeddings = OllamaEmbeddings "llama3.2:latest" Nothing Nothing
                docs = replicate 3 (Document "content" mempty)
            result <- embedDocuments embeddings docs
            case result of
              Left err -> assertFailure $ "Unexpected error: " ++ err
              Right vecs -> assertEqual "Correct number of embeddings" 3 (length vecs)
        ]
    ]
