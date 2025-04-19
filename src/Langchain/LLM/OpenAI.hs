{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : Langchain.LLM.OpenAI
Description : OpenAI integration for LangChain Haskell
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

OpenAI implementation of LangChain's LLM interface. You will have types and chatCompletionFunctions in
  Internal module
-}
module Langchain.LLM.OpenAI
  ( OpenAI (..)
  ) where

import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Langchain.Callback (Callback)
import qualified Langchain.LLM.Core as LLM
import Langchain.LLM.Internal.OpenAI as OpenAI

data OpenAI = OpenAI
  { apiKey :: Text
  , openAIModelName :: Text
  , callbacks :: [Callback]
  }

instance Show OpenAI where
  show OpenAI {..} = "OpenAI " ++ show openAIModelName

instance LLM.LLM OpenAI where
  type LLMParams OpenAI = Text -- Added for removing warning

  generate OpenAI {..} prompt _ = do
    eRes <-
      createChatCompletion
        apiKey
        ( defaultChatCompletionRequest
            { model = openAIModelName
            , messages =
                [ defaultMessage { OpenAI.content = Just (StringContent prompt)}
                {-
                 Message
                    { role = User
                    , content = Just (StringContent prompt)
                    , name = Nothing
                    , functionCall = Nothing
                    , toolCalls = Nothing
                    , toolCallId = Nothing
                    , audio = Nothing
                    , refusal = Nothing
                    }
                    -}
                ]
            }
        )
    case eRes of
      Left err -> return $ Left err
      Right r -> do
        case listToMaybe ((\ChatCompletionResponse {..} -> choices) r) of
          Nothing -> return $ Left "Did not received any response"
          Just resp ->
            let Message {..} = message resp
             in pure $
                  Right $
                    maybe
                      ""
                      ( \c -> case c of
                          StringContent t -> t
                          ContentParts _ -> ""
                      )
                      content
  chat OpenAI {..} msgs _ = do
    eRes <-
      createChatCompletion
        apiKey
        ( defaultChatCompletionRequest
            { model = openAIModelName
            , messages = toOpenAIMessages msgs
            }
        )
    case eRes of
      Left err -> return $ Left err
      Right r -> do
        case listToMaybe ((\ChatCompletionResponse {..} -> choices) r) of
          Nothing -> return $ Left "Did not received any response"
          Just resp ->
            let Message {..} = message resp
             in pure $
                  Right $
                    maybe
                      ""
                      ( \c -> case c of
                          StringContent t -> t
                          ContentParts _ -> ""
                      )
                      content

  stream OpenAI {..} msgs LLM.StreamHandler {onComplete, onToken} _ = do
    let req =
          defaultChatCompletionRequest
            { model = openAIModelName
            , messages = toOpenAIMessages msgs
            , stream = Just True -- Enable streaming
            }
    createChatCompletionStream
      apiKey
      req
      OpenAIStreamHandler
        { onComplete = onComplete
        , onToken = onToken . chunkToText
        }
    where
      chunkToText :: ChatCompletionChunk -> Text
      chunkToText ChatCompletionChunk {..} = do
        case listToMaybe chunkChoices of
          Nothing -> ""
          Just ChunkChoice {..} ->
            fromMaybe "" ((\Delta {..} -> contentForDelta) delta)

toOpenAIMessages :: LLM.ChatMessage -> [Message]
toOpenAIMessages msgs = map go (NE.toList msgs)
  where
    toRole :: LLM.Role -> Role
    toRole r = case r of
      LLM.System -> System
      LLM.User -> User
      LLM.Assistant -> Assistant
      LLM.Tool -> Tool
      LLM.Developer -> Developer
      LLM.Function -> Function

    go :: LLM.Message -> Message
    go msg =
      defaultMessage
        { role = toRole $ LLM.role msg
        , content = Just $ StringContent (LLM.content msg)
        }

{-
ghci> :set -XOverloadedStrings
ghci> let o = OpenAI { apiKey = <my api key>
    , openAIModelName = "gpt-4.1-nano"
    , Langchain.LLM.OpenAI.callbacks = []
    }
ghci> eRes <- generate o "What is 2+2" Nothing
ghci> eRes
Right "2 + 2 equals 4."
ghci> import qualified Data.List.NonEmpty as NE
ghci> let msg = Langchain.LLM.Core.Message Langchain.LLM.Core.User "What is 2+2" defaultMessageData
ghci> let chatMsg = NE.fromList [msg]
ghci> eRes <- chat o chatMsg Nothing
ghci> eRes
Right "2 + 2 equals 4."
-}
