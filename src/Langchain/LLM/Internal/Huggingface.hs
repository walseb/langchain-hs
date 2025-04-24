{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Langchain.LLM.Internal.Huggingface
  ( StreamOptions (..)
  , ChatCompletionRequest (..)
  ) where

import Data.Aeson
import Data.Text (Text)

data ResponseFormat
data Tool
data ToolChoice

-- | Options for streaming responses.
data StreamOptions = StreamOptions
  { includeUsage :: Bool
  -- ^ Whether to include usage information
  }
  deriving (Show, Eq)

instance ToJSON StreamOptions where
  toJSON StreamOptions {..} =
    object
      [ "include_usage" .= includeUsage
      ]

instance FromJSON StreamOptions where
  parseJSON = withObject "StreamOptions" $ \v ->
    StreamOptions <$> v .: "include_usage"

data Role = User | Assistant | Tool | System
  deriving (Eq, Show)

data ImageUrl = ImageUrl {
    url :: String
}

data MessageContent = MessageContent {
    contentType :: Text
  , contentText :: Maybe Text
  , imageUrl :: Maybe ImageUrl
}

data MessageContent_ = MessageContent_ [MessageContent] | Text

data Message = Message
  { role :: Role
  , content :: MessageContent_ 
  , name :: Maybe String
  }

data ChatCompletionRequest = ChatCompletionRequest
  { -- Compulsary fields
    messages :: [Message]
  , model :: Text
  , stream :: Bool
  , -- Optional fields
    maxTokens :: Maybe Double
  , frequencyPenalty :: Maybe Double
  , logProbs :: Maybe Bool
  , presencePenalty :: Maybe Double
  , seed :: Maybe Int
  , stop :: Maybe [String]
  , temperature :: Maybe Double
  , toolPrompt :: Maybe String
  , topLogprobs :: Maybe Int
  , topP :: Maybe Double
  , -- Nested fields
    streamOptions :: Maybe StreamOptions
  , responseFormat :: Maybe ResponseFormat
  , tools :: Maybe [Tool]
  , toolChoice :: Maybe ToolChoice
  }
