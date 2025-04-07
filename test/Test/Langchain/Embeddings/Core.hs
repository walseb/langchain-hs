{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Embeddings.Core (tests) where

-- import Data.Ollama.Embeddings (EmbeddingResp (..))
import Data.Text (isInfixOf, pack)
import Langchain.DocumentLoader.Core
import Langchain.Embeddings.Core
import Langchain.Embeddings.Ollama
import Test.Tasty
import Test.Tasty.HUnit

{-
mockSuccessResponse :: EmbeddingResp
mockSuccessResponse = EmbeddingResp { embedding_ = [[1.0, 2.0, 3.0]] }

mockEmptyResponse :: EmbeddingResp
mockEmptyResponse = EmbeddingResp { embedding_ = [] }
-}

tests :: TestTree
tests =
  testGroup
    "Embedding Tests"
    [ testGroup
        "embedQuery Tests"
        [ testCase "Returns embedding on successful response" $ do
            let embeddings = OllamaEmbeddings "nomic-embed-text:latest" Nothing Nothing
            result <- embedQuery embeddings "test query"
            case result of
              Left err -> assertFailure $ "Expected success, got error: " ++ err
              Right vec -> assertEqual "Correct embedding length" 768 (length vec)
        , {-
          , testCase "Handles empty embedding response" $ do
              let embeddings = OllamaEmbeddings "nomic-embed-text:latest" Nothing Nothing
              -- Assuming embeddingOps returns Right mockEmptyResponse
              result <- embedQuery embeddings "empty query"
              case result of
                Left err -> assertEqual "Correct error message" "Embeddings are empty" err
                Right _ -> assertFailure ("Expected error for empty embedding")
                -}
          testCase "Propagates API errors" $ do
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
            let embeddings = OllamaEmbeddings "nomic-embed-text:latest" Nothing Nothing
                docs = replicate 3 (Document "content" mempty)
            -- Assuming each embeddingOps call returns Right mockSuccessResponse
            result <- embedDocuments embeddings docs
            case result of
              Left err -> assertFailure $ "Unexpected error: " ++ err
              Right vecs -> assertEqual "Correct number of embeddings" 3 (length vecs)
              {-
              , testCase "Handles document processing errors" $ do
                  let embeddings = OllamaEmbeddings "nomic-embed-text:latest" Nothing Nothing
                      docs = [Document "good" mempty, Document "bad" mempty]
                  -- Assuming second embeddingOps call returns Left "Partial Failure"
                  result <- embedDocuments embeddings docs
                  case result of
                    Left err -> assertBool "Error contains 'Partial Failure'" ("Partial Failure" `isInfixOf` (pack err))
                    Right _ -> assertFailure "Expected partial failure error"
              , testCase "Detects empty embeddings in response" $ do
                  let embeddings = OllamaEmbeddings "empty-embed-model" Nothing Nothing
                      docs = [Document "empty" mempty]
                  -- Assuming embeddingOps returns Right mockEmptyResponse
                  result <- embedDocuments embeddings docs
                  case result of
                    Left err -> assertEqual "Correct empty embedding error" "Embeddings are empty" err
                    Right _ -> assertFailure "Expected empty embedding error"
                                  -}
        ]
    ]

