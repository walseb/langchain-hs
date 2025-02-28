{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Langchain.Ollama (Ollama(..)) where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Ollama.Chat as OllamaChat
import qualified Data.Ollama.Generate as OllamaGenerate
import Data.Text (Text)
import Langchain.LLM

type Model = Text
newtype Ollama = Ollama Model

chatRespToText :: OllamaChat.ChatResponse -> Text
chatRespToText OllamaChat.ChatResponse{..} = 
    case message of
      Nothing -> ""
      Just OllamaChat.Message{..} -> content

instance LLM Ollama where
    chat (Ollama model) messages mbParam = do
        eRes <-
            OllamaChat.chat
                OllamaChat.defaultChatOps
                    { OllamaChat.chatModelName = model
                    , OllamaChat.messages = toOllamaMessages messages
                    , OllamaChat.stream = 
                        case mbParam of
                          Nothing -> Nothing
                          Just Params{..} -> 
                            case stream of
                              Nothing -> Nothing
                              Just (onToken, onComplete) -> 
                                Just (onToken . chatRespToText , onComplete)
                    }
        case eRes of
            Left err -> error err
            Right OllamaChat.ChatResponse{message} -> do
                case message of
                    Nothing -> error "got no response"
                    Just OllamaChat.Message{..} -> return content

    call (Ollama model) prompt mbParam = do
        eRes <-
            OllamaGenerate.generate
                OllamaGenerate.defaultGenerateOps
                    { OllamaGenerate.modelName = model
                    , OllamaGenerate.prompt = prompt
                    , OllamaGenerate.stream =
                        case mbParam of
                          Nothing -> Nothing
                          Just Params{..} -> 
                            case stream of
                              Nothing -> Nothing
                              Just (onToken, onComplete) -> 
                                Just (onToken . OllamaGenerate.response_, onComplete)
                    }
        case eRes of
            Left err -> error err
            Right r -> return (OllamaGenerate.response_ r)

toOllamaMessages :: NonEmpty Message -> NonEmpty OllamaChat.Message
toOllamaMessages lst =
    NonEmpty.fromList $
        map
            (\Message{..} -> OllamaChat.Message (toOllamaRole role) content Nothing)
            (NonEmpty.toList lst)
  where
    toOllamaRole User = OllamaChat.User
    toOllamaRole System = OllamaChat.System
    toOllamaRole Assistant = OllamaChat.Assistant
    toOllamaRole Tool = OllamaChat.Tool
