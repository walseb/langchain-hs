{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module:      Langchain.LLM.Huggingface
Copyright:   (c) 2025 Tushar Adhatrao
License:     MIT
Maintainer:  Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability:   experimental

Huggingface inference implementation Langchain's LLM Interface
-}
module Langchain.LLM.Huggingface
  ( Huggingface (..)
  , Huggingface.Provider (..)
  ) where

import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Text (Text, unpack)
import Langchain.Callback
import Langchain.LLM.Core as LLM
import qualified Langchain.LLM.Internal.Huggingface as Huggingface

data Huggingface = Huggingface
  { provider :: Huggingface.Provider
  , apiKey :: Text
  , modelName :: Text
  , callbacks :: [Callback]
  }

instance Show Huggingface where
  show Huggingface {..} =
    "Huggingface { provider = "
      <> show provider
      <> ", modelName = "
      <> unpack modelName
      <> " }"

data HuggingfaceParams = HuggingfaceParams
  { frequencyPenalty :: Maybe Double
  , maxTokens :: Maybe Integer
  , presencePenalty :: Maybe Double
  , stop :: Maybe [String]
  , toolPrompt :: Maybe String
  , topP :: Maybe Double
  , temperature :: Maybe Double
  }
  deriving (Eq, Show)

instance LLM Huggingface where
  type LLMParams Huggingface = HuggingfaceParams

  generate Huggingface {..} prompt mbHuggingfaceParams = do
    eRes <-
      Huggingface.createChatCompletion
        apiKey
        Huggingface.defaultHuggingfaceChatCompletionRequest
          { Huggingface.provider = provider
          , Huggingface.messages =
              [Huggingface.defaultMessage {Huggingface.content = Huggingface.TextContent prompt}]
          , Huggingface.model = modelName
          , Huggingface.stream = False
          , Huggingface.maxTokens = maybe Nothing maxTokens mbHuggingfaceParams
          , Huggingface.frequencyPenalty = maybe Nothing frequencyPenalty mbHuggingfaceParams
          , -- , Huggingface.logProbs = maybe Nothing logProbs mbHuggingfaceParams
            Huggingface.presencePenalty = maybe Nothing presencePenalty mbHuggingfaceParams
          , -- , Huggingface.seed = maybe Nothing seed mbHuggingfaceParams
            Huggingface.stop = maybe Nothing stop mbHuggingfaceParams
          , Huggingface.temperature = maybe Nothing temperature mbHuggingfaceParams
          , -- , Huggingface.toolPrompt = maybe Nothing toolPrompt mbHuggingfaceParams
            -- , Huggingface.topLogprobs = maybe Nothing topLogProbs mbHuggingfaceParams
            Huggingface.topP = maybe Nothing topP mbHuggingfaceParams
            -- , Huggingface.streamOptions = maybe Nothing streamOptions mbHuggingfaceParams
            -- , Huggingface.responseFormat = maybe Nothing responseFormat mbHuggingfaceParams
            -- , Huggingface.tools = maybe Nothing tools mbHuggingfaceParams
            -- , Huggingface.toolChoice = maybe Nothing toolChoice mbHuggingfaceParams
          }
    case eRes of
      Left err -> return $ Left err
      Right r -> do
        case listToMaybe ((\Huggingface.ChatCompletionResponse {..} -> choices) r) of
          Nothing -> return $ Left "Did not received any response"
          Just resp ->
            let Huggingface.Message {..} = Huggingface.message resp
             in pure $
                  Right $
                    ( \c -> case c of
                        Huggingface.TextContent t -> t
                        _ -> ""
                    )
                      content

  chat Huggingface {..} msgs mbHuggingfaceParams = do
    eRes <-
      Huggingface.createChatCompletion
        apiKey
        Huggingface.defaultHuggingfaceChatCompletionRequest
          { Huggingface.provider = provider
          , Huggingface.messages = toHuggingfaceMessages msgs
          , Huggingface.model = modelName
          , Huggingface.stream = False
          , Huggingface.maxTokens = maybe Nothing maxTokens mbHuggingfaceParams
          , Huggingface.frequencyPenalty = maybe Nothing frequencyPenalty mbHuggingfaceParams
          , -- , Huggingface.logProbs = maybe Nothing logProbs mbHuggingfaceParams
            Huggingface.presencePenalty = maybe Nothing presencePenalty mbHuggingfaceParams
          , -- , Huggingface.seed = maybe Nothing seed mbHuggingfaceParams
            Huggingface.stop = maybe Nothing stop mbHuggingfaceParams
          , Huggingface.temperature = maybe Nothing temperature mbHuggingfaceParams
          , -- , Huggingface.toolPrompt = maybe Nothing toolPrompt mbHuggingfaceParams
            -- , Huggingface.topLogprobs = maybe Nothing topLogProbs mbHuggingfaceParams
            Huggingface.topP = maybe Nothing topP mbHuggingfaceParams
            -- , Huggingface.streamOptions = maybe Nothing streamOptions mbHuggingfaceParams
            -- , Huggingface.responseFormat = maybe Nothing responseFormat mbHuggingfaceParams
            -- , Huggingface.tools = maybe Nothing tools mbHuggingfaceParams
            -- , Huggingface.toolChoice = maybe Nothing toolChoice mbHuggingfaceParams
          }
    case eRes of
      Left err -> return $ Left err
      Right r -> do
        case listToMaybe ((\Huggingface.ChatCompletionResponse {..} -> choices) r) of
          Nothing -> return $ Left "Did not received any response"
          Just resp ->
            let Huggingface.Message {..} = Huggingface.message resp
             in pure $
                  Right $
                    ( \c -> case c of
                        Huggingface.TextContent t -> t
                        _ -> ""
                    )
                      content

  stream Huggingface {..} msgs LLM.StreamHandler {..} mbHuggingfaceParams = do
    Huggingface.createChatCompletionStream
      apiKey
      Huggingface.defaultHuggingfaceChatCompletionRequest
        { Huggingface.provider = provider
        , Huggingface.messages = toHuggingfaceMessages msgs
        , Huggingface.model = modelName
        , Huggingface.stream = False
        , Huggingface.maxTokens = maybe Nothing maxTokens mbHuggingfaceParams
        , Huggingface.frequencyPenalty = maybe Nothing frequencyPenalty mbHuggingfaceParams
        , -- , Huggingface.logProbs = maybe Nothing logProbs mbHuggingfaceParams
          Huggingface.presencePenalty = maybe Nothing presencePenalty mbHuggingfaceParams
        , -- , Huggingface.seed = maybe Nothing seed mbHuggingfaceParams
          Huggingface.stop = maybe Nothing stop mbHuggingfaceParams
        , Huggingface.temperature = maybe Nothing temperature mbHuggingfaceParams
        , -- , Huggingface.toolPrompt = maybe Nothing toolPrompt mbHuggingfaceParams
          -- , Huggingface.topLogprobs = maybe Nothing topLogProbs mbHuggingfaceParams
          Huggingface.topP = maybe Nothing topP mbHuggingfaceParams
          -- , Huggingface.streamOptions = maybe Nothing streamOptions mbHuggingfaceParams
          -- , Huggingface.responseFormat = maybe Nothing responseFormat mbHuggingfaceParams
          -- , Huggingface.tools = maybe Nothing tools mbHuggingfaceParams
          -- , Huggingface.toolChoice = maybe Nothing toolChoice mbHuggingfaceParams
        }
      Huggingface.HuggingfaceStreamHandler
        { Huggingface.onComplete = onComplete
        , Huggingface.onToken = onToken . chunkToText
        }
    where
      chunkToText :: Huggingface.ChatCompletionChunk -> Text
      chunkToText Huggingface.ChatCompletionChunk {..} = do
        case listToMaybe chunkChoices of
          Nothing -> ""
          Just Huggingface.ChoiceChunk {..} ->
            fromMaybe "" ((\Huggingface.Delta {..} -> deltaContent) delta)

toHuggingfaceMessages :: LLM.ChatMessage -> [Huggingface.Message]
toHuggingfaceMessages msgs = map go (NE.toList msgs)
  where
    toRole :: LLM.Role -> Huggingface.Role
    toRole r = case r of
      LLM.System -> Huggingface.System
      LLM.User -> Huggingface.User
      LLM.Assistant -> Huggingface.Assistant
      LLM.Tool -> Huggingface.Tool
      _ -> Huggingface.System
    -- LLM.Developer -> Huggingface.Developer
    -- LLM.Function -> Huggingface.Function

    go :: LLM.Message -> Huggingface.Message
    go msg =
      Huggingface.defaultMessage
        { Huggingface.role = toRole $ LLM.role msg
        , Huggingface.content = Huggingface.TextContent (LLM.content msg)
        }
