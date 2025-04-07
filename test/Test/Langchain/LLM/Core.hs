{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Langchain.LLM.Core where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Aeson (encode, decode, toJSON, fromJSON, Result(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.List.NonEmpty (NonEmpty(..))
import Langchain.LLM.Core

-- Example implementation of LLM for testing
data TestLLM = TestLLM
  { responseText :: Text
  , shouldSucceed :: Bool
  }

instance LLM TestLLM where
  generate m _ params =
    pure $ if shouldSucceed m
      then Right (responseText m)
      else Left "Test error"
      
  chat m _ params =
    pure $ if shouldSucceed m
      then Right (responseText m)
      else Left "Test error"
      
  stream m _ handler params = do
    if shouldSucceed m
      then do
        onToken handler (responseText m)
        onComplete handler
        pure (Right ())
      else pure (Left "Test error")

tests :: TestTree
tests = testGroup "LLMCoreTest"
  [ testGroup "Params"
      [ testCase "creates default parameters with all Nothing fields" $ do
          let params = defaultParams
          assertEqual "temperature should be Nothing" Nothing (temperature params)
          assertEqual "maxTokens should be Nothing" Nothing (maxTokens params)
          assertEqual "topP should be Nothing" Nothing (topP params)
          assertEqual "n should be Nothing" Nothing (n params)
          assertEqual "stop should be Nothing" Nothing (stop params)
      , testCase "can override default parameters" $ do
          let params = defaultParams { temperature = Just 0.7, maxTokens = Just 100 }
          assertEqual "temperature should be Just 0.7" (Just 0.7) (temperature params)
          assertEqual "maxTokens should be Just 100" (Just 100) (maxTokens params)
          assertEqual "topP should be Nothing" Nothing (topP params)
      ]
  , testGroup "Role"
      [ testCase "has correct equality" $ do
          assertEqual "System equals System" System System
          assertEqual "User equals User" User User
          assertEqual "Assistant equals Assistant" Assistant Assistant
          assertEqual "Tool equals Tool" Tool Tool
          assertBool "System should not equal User" (System /= User)
      , testCase "can be converted to and from JSON" $ do
          case fromJSON (toJSON System) of
            Success r -> assertEqual "JSON roundtrip for System" System r
            _         -> assertFailure "JSON conversion failed for System"
          case fromJSON (toJSON User) of
            Success r -> assertEqual "JSON roundtrip for User" User r
            _         -> assertFailure "JSON conversion failed for User"
          case fromJSON (toJSON Assistant) of
            Success r -> assertEqual "JSON roundtrip for Assistant" Assistant r
            _         -> assertFailure "JSON conversion failed for Assistant"
          case fromJSON (toJSON Tool) of
            Success r -> assertEqual "JSON roundtrip for Tool" Tool r
            _         -> assertFailure "JSON conversion failed for Tool"
      ]
  , testGroup "Message"
      [ testCase "creates messages with correct fields" $ do
          let msg = Message User "Hello" defaultMessageData
          assertEqual "role should be User" User (role msg)
          assertEqual "content should be 'Hello'" "Hello" (content msg)
          assertEqual "messageData should be default" defaultMessageData (messageData msg)
      , testCase "creates messages with custom message data" $ do
          let customData = defaultMessageData { name = Just "Alice" }
          let msg = Message User "Hello" customData
          assertEqual "role should be User" User (role msg)
          assertEqual "content should be 'Hello'" "Hello" (content msg)
          assertEqual "name should be Just 'Alice'" (Just "Alice") (name (messageData msg))
          assertEqual "toolCalls should be Nothing" Nothing (toolCalls (messageData msg))
      ]
  , testGroup "MessageData"
      [ testCase "creates default message data with all Nothing fields" $ do
          let md = defaultMessageData
          assertEqual "name should be Nothing" Nothing (name md)
          assertEqual "toolCalls should be Nothing" Nothing (toolCalls md)
      , testCase "serializes to correct JSON structure" $ do
          let md = MessageData (Just "Alice") (Just ["tool1", "tool2"])
              expected = "{\"name\":\"Alice\",\"tool_calls\":[\"tool1\",\"tool2\"]}"
          assertEqual "JSON encoding of MessageData" expected (encode md)
      , testCase "deserializes from JSON correctly" $ do
          let json = "{\"name\":\"Bob\",\"tool_calls\":[\"tool3\"]}"
              expected = MessageData (Just "Bob") (Just ["tool3"])
          assertEqual "JSON decoding of MessageData" (Just expected) (decode json)
      , testCase "handles partial JSON correctly" $ do
          let json = "{\"name\":\"Charlie\"}"
              expected = MessageData (Just "Charlie") Nothing
          assertEqual "Partial JSON decoding of MessageData" (Just expected) (decode json)
      ]
  , testGroup "LLM Typeclass"
      [ testGroup "generate"
          [ testCase "returns Right with response for successful generation" $ do
              let successLLM = TestLLM "Success response" True
              result <- generate successLLM "Test prompt" Nothing
              assertEqual "Successful generation" (Right "Success response") result
          , testCase "returns Left with error for failed generation" $ do
              let failureLLM = TestLLM "Failure response" False
              result <- generate failureLLM "Test prompt" Nothing
              assertEqual "Failed generation" (Left "Test error") result
          , testCase "works with custom parameters" $ do
              let successLLM = TestLLM "Success response" True
                  params = defaultParams { temperature = Just 0.5 }
              result <- generate successLLM "Test prompt" (Just params)
              assertEqual "Generation with custom params" (Right "Success response") result
          ]
      , testGroup "chat"
          [ testCase "returns Right with response for successful chat" $ do
              let successLLM = TestLLM "Success response" True
                  singleMsg = Message User "Test prompt" defaultMessageData
                  chatMsgs = singleMsg :| []
              result <- chat successLLM chatMsgs Nothing
              assertEqual "Successful chat" (Right "Success response") result
          , testCase "returns Left with error for failed chat" $ do
              let failureLLM = TestLLM "Failure response" False
                  singleMsg = Message User "Test prompt" defaultMessageData
                  chatMsgs = singleMsg :| []
              result <- chat failureLLM chatMsgs Nothing
              assertEqual "Failed chat" (Left "Test error") result
          ]
      , testGroup "stream"
          [ testCase "calls handlers and returns Right for successful stream" $ do
              let successLLM = TestLLM "Success response" True
                  singleMsg = Message User "Test prompt" defaultMessageData
                  chatMsgs = singleMsg :| []
                  handler = StreamHandler
                    { onToken = \_ -> pure ()
                    , onComplete = pure ()
                    }
              result <- stream successLLM chatMsgs handler Nothing
              assertEqual "Successful stream" (Right ()) result
          , testCase "returns Left with error for failed stream" $ do
              let failureLLM = TestLLM "Failure response" False
                  singleMsg = Message User "Test prompt" defaultMessageData
                  chatMsgs = singleMsg :| []
                  handler = StreamHandler
                    { onToken = \_ -> pure ()
                    , onComplete = pure ()
                    }
              result <- stream failureLLM chatMsgs handler Nothing
              assertEqual "Failed stream" (Left "Test error") result
          ]
      ]
  , testGroup "ChatMessage"
      [ testCase "creates non-empty list of messages" $ do
          let msg1 = Message User "Hello" defaultMessageData
              msg2 = Message Assistant "Hi there" defaultMessageData
              chat = msg1 :| [msg2]
          assertEqual "ChatMessage length" 2 (length chat)
      ]
  ]
