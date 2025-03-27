{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | This module provides an implementation of the 'LLM' typeclass for the Ollama
-- language model backend.
module Langchain.LLM.Ollama (Ollama (..)) where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Ollama.Chat as OllamaChat
import qualified Data.Ollama.Generate as OllamaGenerate
import Data.Text (Text)
import Langchain.LLM.Core

-- | A wrapper around the model name for the Ollama language model.
newtype Ollama = Ollama Text -- ^ The name of the Ollama model
  deriving (Show)

-- | Implementation of the 'LLM' typeclass for the Ollama backend.
-- Note that the 'Params' argument is currently ignored in all methods.
instance LLM Ollama where
  
  -- | Invoke the Ollama model with a single prompt.
  -- Returns either an error or the generated text. Ignores 'Params'.
  invoke (Ollama model) prompt _ = do
    eRes <-
      OllamaGenerate.generate
        OllamaGenerate.defaultGenerateOps
          { OllamaGenerate.modelName = model
          , OllamaGenerate.prompt = prompt
          , OllamaGenerate.stream = Nothing
          }
    return $ either Left (Right . OllamaGenerate.response_) eRes

  -- | Chat with the Ollama model using a sequence of messages.
  -- Returns either an error or the response text. Ignores 'Params' for now.
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

  -- | Stream responses from the Ollama model for a sequence of messages.
  -- Uses 'StreamHandler' callbacks for real-time processing. Ignores 'Params' for now.
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

-- | Convert a list of Langchain 'Message's to Ollama 'Message's.
-- | Currently ignores the 'messageData' field, as it is not supported by Ollama.
-- TODO: Receive tool_calls from Ollama
toOllamaMessages :: NonEmpty Message -> NonEmpty OllamaChat.Message
toOllamaMessages = NonEmpty.map $ \Message {..} ->
  OllamaChat.Message (toOllamaRole role) content Nothing Nothing
  where
    toOllamaRole User = OllamaChat.User
    toOllamaRole System = OllamaChat.System
    toOllamaRole Assistant = OllamaChat.Assistant
    toOllamaRole Tool = OllamaChat.Tool
