{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

{- | This module provides an implementation of the 'LLM' typeclass for the Ollama
language model backend.
-}
module Langchain.LLM.Ollama (Ollama (..)) where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Ollama.Chat as OllamaChat
import qualified Data.Ollama.Generate as OllamaGenerate
import Data.Text (Text)
import Langchain.Callback (Callback, Event (..))
import Langchain.LLM.Core
import qualified Langchain.Runnable.Core as Run

-- | A wrapper around the model name for the Ollama language model.
data Ollama = Ollama
  { modelName :: Text
  -- ^ The name of the Ollama model
  , callbacks :: [Callback]
  -- ^ Callbacks for streaming responses
  }

instance Show Ollama where
  show (Ollama modelName _) = "Ollama " ++ show modelName

{- | Implementation of the 'LLM' typeclass for the Ollama backend.
Note that the 'Params' argument is currently ignored in all methods.
-}
instance LLM Ollama where
  -- \| Invoke the Ollama model with a single prompt.
  -- Returns either an error or the generated text. Ignores 'Params'.
  -- TODO: Pass down params to Ollama generate
  generate (Ollama model cbs) prompt _ = do
    mapM_ (\cb -> cb LLMStart) cbs
    eRes <-
      OllamaGenerate.generate
        OllamaGenerate.defaultGenerateOps
          { OllamaGenerate.modelName = model
          , OllamaGenerate.prompt = prompt
          , OllamaGenerate.stream = Nothing
          }
    case eRes of
      Left err -> do
        mapM_ (\cb -> cb (LLMError err)) cbs
        return $ Left (show err)
      Right res -> do
        mapM_ (\cb -> cb LLMEnd) cbs
        return $ Right (OllamaGenerate.response_ res)

  -- \| Chat with the Ollama model using a sequence of messages.
  -- Returns either an error or the response text. Ignores 'Params' for now.
  -- TODO: Pass down params to Ollama chat
  chat (Ollama model cbs) messages _ = do
    mapM_ (\cb -> cb LLMStart) cbs
    eRes <-
      OllamaChat.chat
        OllamaChat.defaultChatOps
          { OllamaChat.chatModelName = model
          , OllamaChat.messages = toOllamaMessages messages
          , OllamaChat.stream = Nothing
          }
    case eRes of
      Left err -> do
        mapM_ (\cb -> cb (LLMError err)) cbs
        return $ Left (show err)
      Right res -> do
        mapM_ (\cb -> cb LLMEnd) cbs
        return $ Right (chatRespToText res)
    where
      chatRespToText resp = maybe "" OllamaChat.content (OllamaChat.message resp)

  -- \| Stream responses from the Ollama model for a sequence of messages.
  -- Uses 'StreamHandler' callbacks for real-time processing. Ignores 'Params' for now.
  stream (Ollama model_ cbs) messages StreamHandler {onToken, onComplete} _ = do
    mapM_ (\cb -> cb LLMStart) cbs
    eRes <-
      OllamaChat.chat
        OllamaChat.defaultChatOps
          { OllamaChat.chatModelName = model_
          , OllamaChat.messages = toOllamaMessages messages
          , OllamaChat.stream = Just (onToken . chatRespToText, onComplete)
          }
    case eRes of
      Left err -> do
        mapM_ (\cb -> cb (LLMError err)) cbs
        return $ Left (show err)
      Right _ -> do
        mapM_ (\cb -> cb LLMEnd) cbs
        return $ Right ()
    where
      chatRespToText OllamaChat.ChatResponse {..} = maybe "" OllamaChat.content message

{- | Convert a list of Langchain 'Message's to Ollama 'Message's.
| Currently ignores the 'messageData' field, as it is not supported by Ollama.
TODO: Receive tool_calls from Ollama
-}
toOllamaMessages :: NonEmpty Message -> NonEmpty OllamaChat.Message
toOllamaMessages = NonEmpty.map $ \Message {..} ->
  OllamaChat.Message (toOllamaRole role) content Nothing Nothing
  where
    toOllamaRole User = OllamaChat.User
    toOllamaRole System = OllamaChat.System
    toOllamaRole Assistant = OllamaChat.Assistant
    toOllamaRole Tool = OllamaChat.Tool

instance Run.Runnable Ollama where
  type RunnableInput Ollama = ChatMessage
  type RunnableOutput Ollama = Text

  -- TODO: need to figure out a way to pass mbParams
  invoke model input = chat model input Nothing
