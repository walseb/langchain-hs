{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Langchain.LLM.Ollama where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Concurrent
import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (liftIO)
import Data.IORef
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)
import qualified Data.Text as T

import Langchain.Callback (Callback, Event (..))
import Langchain.LLM.Core
import Langchain.LLM.Ollama
import qualified Langchain.Runnable.Core as Run

-- | Helper to capture callback events
captureEvents :: IO (Callback, IO [Event])
captureEvents = do
  eventsRef <- newIORef []
  let callback event = modifyIORef eventsRef (event :)
  let getEvents = reverse <$> readIORef eventsRef
  return (callback, getEvents)

-- | Test model name
testModelName :: Text
testModelName = "llama3.2:latest"

-- | Main test tree
tests :: TestTree
tests =
  testGroup
    "Ollama"
    [ testCase "Show instance formats Ollama correctly" $ do
        let ollama = Ollama "llama2" []
        show ollama @?= "Ollama \"llama2\""
    , testCase "generate returns text response for a prompt" $ do
        (callback, getEvents) <- captureEvents
        let ollama = Ollama testModelName [callback]
        let prompt = "What is functional programming?"
        result <- generate ollama prompt Nothing
        case result of
          Left err -> assertFailure $ "Expected success, got error: " ++ err
          Right response -> do
            assertBool "Non-empty response expected" (T.length response > 0)
            events <- getEvents
            assertBool
              "should contain all events"
              (events `shouldContainAll` [LLMStart, LLMEnd])
    , testCase "generate returns error for invalid model" $ do
        (callback, getEvents) <- captureEvents
        let ollama = Ollama "non_existent_model" [callback]
        let prompt = "Hello"
        result <- generate ollama prompt Nothing
        case result of
          Left err -> do
            assertBool "Error should mention model" ("non_existent_model" `T.isInfixOf` T.pack err)
            events <- getEvents
            assertBool "LLM should tried to be started" (events `shouldContainAll` [LLMStart])
            length (filter isErrorEvent events) @?= 1
          Right _ -> assertFailure "Expected error, but got success"
    , testCase "chat returns text response for messages" $ do
        (callback, getEvents) <- captureEvents
        let ollama = Ollama testModelName [callback]
        let messages = Message User "What's the capital of France?" defaultMessageData :| []
        result <- chat ollama messages Nothing
        case result of
          Left err -> assertFailure $ "Expected success, got error: " ++ err
          Right response -> do
            assertBool "Response should mention Paris" ("paris" `T.isInfixOf` T.toLower response)
            events <- getEvents
            assertBool "LLM should be completed" (events `shouldContainAll` [LLMStart, LLMEnd])
    , testCase "chat handles multi-turn conversations" $ do
        (callback, _) <- captureEvents
        let ollama = Ollama testModelName [callback]
        let messages =
              Message System "You are a helpful assistant." defaultMessageData
                :| [ Message User "What's the capital of France?" defaultMessageData
                   , Message Assistant "The capital of France is Paris." defaultMessageData
                   , Message User "And what about Italy?" defaultMessageData
                   ]
        result <- chat ollama messages Nothing
        case result of
          Left err -> assertFailure $ "Expected success, got error: " ++ err
          Right response -> assertBool "Response should mention Rome" ("rome" `T.isInfixOf` T.toLower response)
    , testCase "stream calls handlers for streaming responses" $ do
        let ollama = Ollama testModelName []
        let messages = Message User "Count from 1 to 5 briefly." defaultMessageData :| []

        tokensRef <- newIORef []
        completedRef <- newIORef False

        let handler =
              StreamHandler
                { onToken = \token -> modifyIORef tokensRef (token :)
                , onComplete = writeIORef completedRef True
                }

        result <- stream ollama messages handler Nothing
        case result of
          Left err -> assertFailure $ "Expected success, got error: " ++ err
          Right () -> do
            tokens <- readIORef tokensRef
            assertBool "Should receive tokens" (not (null tokens))
            completed <- readIORef completedRef
            completed @?= True
    , testCase "invoke calls chat with the input messages" $ do
        let ollama = Ollama testModelName []
        let input = Message User "What is 2+2?" defaultMessageData :| []
        result <- Run.invoke ollama input
        case result of
          Left err -> assertFailure $ "Expected success, got error: " ++ err
          Right response -> assertBool "Should mention 4" ("4" `T.isInfixOf` T.toLower response)
    ]
  where
    isErrorEvent (LLMError _) = True
    isErrorEvent _ = False

    shouldContainAll xs ys = all (`elem` xs) ys
