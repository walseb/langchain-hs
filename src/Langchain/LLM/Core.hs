{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- | This module defines the core types and typeclasses for the Langchain library,
providing a standardized interface for interacting with language models.
-}
module Langchain.LLM.Core
  ( Params (..)
  , StreamHandler (..)
  , LLM (..)
  , defaultParams
  , Message (..)
  , Role (..)
  , defaultMessageData
  , ChatMessage
  ) where

import Data.Aeson
import Data.List.NonEmpty
import Data.Text (Text)
import GHC.Generics

{- | Parameters for configuring language model invocations.
These parameters control aspects such as the randomness, length, and stopping
conditions of the generated output.
https://python.langchain.com/docs/concepts/chat_models/#standard-parameters
-}
data Params = Params
  { temperature :: Maybe Double
  -- ^ Sampling temperature, controls randomness (higher = more creative)
  , maxTokens :: Maybe Integer
  -- ^ Maximum number of tokens to generate
  , topP :: Maybe Double
  -- ^ Nucleus sampling parameter (top-p probability mass)
  , n :: Maybe Int
  -- ^ Number of responses to generate
  , stop :: Maybe [Text]
  -- ^ Sequences where generation should stop
  }
  deriving (Show, Eq)

{- | Callbacks for handling streaming responses from a language model.
This allows real-time processing of tokens as they are generated and an action
upon completion.
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
-}
class LLM m where
  -- | Invoke the language model with a single prompt.
  -- Returns either an error message or the generated text.
  invoke :: m -> Text -> Maybe Params -> IO (Either String Text)

  -- | Chat with the language model using a sequence of messages.
  -- Suitable for multi-turn conversations; returns either an error or the response.
  chat :: m -> ChatMessage -> Maybe Params -> IO (Either String Text)

  -- | Stream responses from the language model for a sequence of messages.
  -- Uses callbacks to process tokens in real-time; returns either an error or unit.
  stream :: m -> ChatMessage -> StreamHandler -> Maybe Params -> IO (Either String ())

{- | Default parameters with all fields set to Nothing.
Use this when no specific configuration is needed for the language model.
-}
defaultParams :: Params
defaultParams =
  Params
    { temperature = Nothing
    , maxTokens = Nothing
    , topP = Nothing
    , n = Nothing
    , stop = Nothing
    }
