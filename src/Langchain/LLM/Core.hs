{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module:      Langchain.LLM.Core
Copyright:   (c) 2025 Tushar Adhatrao
License:     MIT
Maintainer:  Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability:   experimental

This module provides the core types and typeclasses for the Langchain library in Haskell,
which is designed to facilitate interaction with language models (LLMs). It defines a standardized
interface that allows different LLM implementations to be used interchangeably, promoting code reuse
and modularity.

The main components include:

* The 'LLM' typeclass, which defines the interface for language models.
* Data types such as 'Params' for configuring model invocations, 'Message' for conversation messages,
  and 'StreamHandler' for handling streaming responses.
* Default values like 'defaultParams' and 'defaultMessageData' for convenience.

This module is intended to be used as the foundation for building applications that interact with LLMs,
providing a consistent API across different model implementations.
-}
module Langchain.LLM.Core
  ( -- * LLM Typeclass
    LLM (..)

    -- * Parameters
  , Message (..)
  , Role (..)
  , ChatMessage
  , MessageData (..)
  , StreamHandler (..)

    -- * Default Values
  , defaultMessageData
  ) where

import Data.Aeson
import Data.List.NonEmpty
import Data.Text (Text)
import GHC.Generics

{- | Callbacks for handling streaming responses from a language model.
This allows real-time processing of tokens as they are generated and an action
upon completion.

@
printHandler :: StreamHandler
printHandler = StreamHandler
  { onToken = putStrLn . ("Token: " ++)
  , onComplete = putStrLn "Streaming complete"
  }
@
-}
data StreamHandler = StreamHandler
  { onToken :: Text -> IO ()
  -- ^ Action to perform for each token received
  , onComplete :: IO ()
  -- ^ Action to perform when streaming is complete
  }

-- | Enumeration of possible roles in a conversation.
data Role
  = -- | System role, typically for instructions or context
    System
  | -- | User role, for user inputs
    User
  | -- | Assistant role, for model responses
    Assistant
  | -- | Tool role, for tool outputs or interactions
    Tool
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

{- | Represents a message in a conversation, including the sender's role, content,
and additional metadata.
https://python.langchain.com/docs/concepts/messages/

@
userMsg :: Message
userMsg = Message
  { role = User
  , content = "Explain functional programming"
  , messageData = defaultMessageData
  }
@
-}
data Message = Message
  { role :: Role
  -- ^ The role of the message sender
  , content :: Text
  -- ^ The content of the message
  , messageData :: MessageData
  -- ^ Additional data associated with the message
  }
  deriving (Eq, Show)

{- | Additional data for a message, such as a name or tool calls.
This type is designed for extensibility, allowing new fields to be added without
breaking changes. Use 'defaultMessageData' for typical usage.
-}
data MessageData = MessageData
  { name :: Maybe Text
  -- ^ Optional name associated with the message
  , toolCalls :: Maybe [Text]
  -- ^ Optional list of tool calls invoked by the message
  }
  deriving (Eq, Show)

-- | JSON serialization for MessageData.
instance ToJSON MessageData where
  toJSON MessageData {..} =
    object
      [ "name" .= name
      , "tool_calls" .= toolCalls
      -- Add more fields as they are added
      ]

-- | JSON deserialization for MessageData.
instance FromJSON MessageData where
  parseJSON = withObject "MessageData" $ \v ->
    MessageData
      <$> v .:? "name"
      <*> v .:? "tool_calls"

-- | Type alias for NonEmpty Message
type ChatMessage = NonEmpty Message

{- | Default message data with all fields set to Nothing.
Use this for standard messages without additional metadata
-}
defaultMessageData :: MessageData
defaultMessageData =
  MessageData
    { name = Nothing
    , toolCalls = Nothing
    }

{- | Typeclass defining the interface for language models.
This provides methods for invoking the model, chatting with it, and streaming
responses.

@
data TestLLM = TestLLM
  { responseText :: Text
  , shouldSucceed :: Bool
  }

instance LLM TestLLM where
  generate m _ _ = pure $ if shouldSucceed m
    then Right (responseText m)
    else Left "Test error"
@


@
ollamaLLM = Ollama "gemma3:latest" [stdOutCallback]
response <- generate ollamaLLM "What is Haskell?" Nothing
@
-}
class LLM m where
  -- | Invoke the language model with a single prompt.
  --        Suitable for simple queries; returns either an error or generated text.
  type LLMParams m

  {- === Using 'generate'
  To invoke an LLM with a single prompt:
  
  @
  let myLLM = ... -- assume this is an instance of LLM
  result <- generate myLLM "What is the meaning of life?" Nothing
  case result of
    Left err -> putStrLn $ "Error: " ++ err
    Right response -> putStrLn response
  @

  -}
  generate :: m -- ^ The type of the language model instance.
    -> Text -- ^ The prompt to send to the model.
    -> Maybe (LLMParams m) -- ^ Optional configuration parameters.
    -> IO (Either String Text)

  -- | Chat with the language model using a sequence of messages.
  -- Suitable for multi-turn conversations; returns either an error or the response.
  --
  chat :: m -- ^ The type of the language model instance.
    -> ChatMessage -- ^ A non-empty list of messages to send to the model.
    -> Maybe (LLMParams m) -- ^ Optional configuration parameters.
    -> IO (Either String Text) -- ^ The result of the chat, either an error or the response text.

  -- | Stream responses from the language model for a sequence of messages.
  -- Uses callbacks to process tokens in real-time; returns either an error or unit.
  stream :: m -> ChatMessage -> StreamHandler -> Maybe (LLMParams m) -> IO (Either String ())
