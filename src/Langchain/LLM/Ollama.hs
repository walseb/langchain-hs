{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Langchain.LLM.Ollama (Ollama (..)) where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Ollama.Chat as OllamaChat
import qualified Data.Ollama.Generate as OllamaGenerate
import Data.Text (Text)
import Langchain.LLM.Core

newtype Ollama = Ollama Text -- Model name
  deriving (Show)

instance LLM Ollama where
  invoke (Ollama model) prompt _ = do
    eRes <-
      OllamaGenerate.generate
        OllamaGenerate.defaultGenerateOps
          { OllamaGenerate.modelName = model
          , OllamaGenerate.prompt = prompt
          , OllamaGenerate.stream = Nothing
          }
    return $ either Left (Right . OllamaGenerate.response_) eRes

  chat (Ollama model) messages _ = do
    eRes <-
      OllamaChat.chat
        OllamaChat.defaultChatOps
          { OllamaChat.chatModelName = model
          , OllamaChat.messages = toOllamaMessages messages
          , OllamaChat.stream = Nothing
          }
    return $ either Left (Right . chatRespToText) eRes
    where
      chatRespToText resp = maybe "" OllamaChat.content (OllamaChat.message resp)

  stream (Ollama model_) messages StreamHandler {onToken, onComplete} _ = do
    eRes <-
      OllamaChat.chat
        OllamaChat.defaultChatOps
          { OllamaChat.chatModelName = model_
          , OllamaChat.messages = toOllamaMessages messages
          , OllamaChat.stream = Just (onToken . chatRespToText, onComplete)
          }
    return $ either Left (const $ Right ()) eRes
    where
      chatRespToText OllamaChat.ChatResponse {..} = maybe "" OllamaChat.content message

toOllamaMessages :: NonEmpty Message -> NonEmpty OllamaChat.Message
toOllamaMessages = NonEmpty.map $ \Message {..} ->
  OllamaChat.Message (toOllamaRole role) content Nothing Nothing
  where
    toOllamaRole User = OllamaChat.User
    toOllamaRole System = OllamaChat.System
    toOllamaRole Assistant = OllamaChat.Assistant
    toOllamaRole Tool = OllamaChat.Tool
