{-# LANGUAGE RecordWildCards #-}

module Langchain.Ollama (
    Ollama (..),
) where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Ollama.Chat as Ollama
import qualified Data.Ollama.Generate as Ollama
import Data.Text (Text)
import Langchain.LLM

type Model = Text
newtype Ollama = Ollama Model

instance LLM Ollama where
    chat (Ollama model) messages _ = do
        eRes <-
            Ollama.chat
                Ollama.defaultChatOps
                    { Ollama.chatModelName = model
                    , Ollama.messages = toOllamaMessages messages
                    }
        case eRes of
            Left err -> error err
            Right Ollama.ChatResponse{..} -> do
                case message of
                    Nothing -> error "got no response"
                    Just Ollama.Message{..} -> return content

    call (Ollama model) prompt _ = do
        eRes <-
            Ollama.generate
                Ollama.defaultGenerateOps
                    { Ollama.modelName = model
                    , Ollama.prompt = prompt
                    }
        case eRes of
            Left err -> error err
            Right r -> return (Ollama.response_ r)

toOllamaMessages :: NonEmpty Message -> NonEmpty Ollama.Message
toOllamaMessages lst =
    NonEmpty.fromList $
        map
            (\Message{..} -> Ollama.Message Ollama.User content Nothing)
            (NonEmpty.toList lst)
