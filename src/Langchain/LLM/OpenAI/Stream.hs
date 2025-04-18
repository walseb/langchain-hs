{-# LANGUAGE OverloadedStrings #-}

module Langchain.LLM.OpenAI.Stream (
    ChatCompletionChunk (..)
  , ChunkChoice (..)
  , Delta (..)
  ) where

import Langchain.LLM.OpenAI.Types (FinishReason)
import Data.Aeson
import Data.Text (Text)

data ChatCompletionChunk = ChatCompletionChunk
  { chunkChoices :: [ChunkChoice]
  } deriving (Show)

instance FromJSON ChatCompletionChunk where
  parseJSON = withObject "ChatCompletionChunk" $ \v ->
    ChatCompletionChunk <$> v .: "choices"

data ChunkChoice = ChunkChoice
  { delta :: Delta
  , finishReason :: Maybe FinishReason
  } deriving (Show )

instance FromJSON ChunkChoice where
  parseJSON = withObject "ChunkChoice" $ \v ->
    ChunkChoice <$> v .: "delta" <*> v .:? "finish_reason"

data Delta = Delta
  { content :: Maybe Text
  } deriving (Show)

instance FromJSON Delta where
  parseJSON = withObject "Delta" $ \v ->
    Delta <$> v .:? "content"
