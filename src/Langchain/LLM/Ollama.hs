{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : Langchain.LLM.Ollama
Description : Ollama integration for LangChain Haskell
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Ollama implementation of LangChain's LLM interface , supporting:

- Text generation
- Chat interactions
- Streaming responses
- Callback integration

Example usage:

@
-- Create Ollama configuration
ollamaLLM = Ollama "llama3" [stdOutCallback]

-- Generate text
response <- generate ollamaLLM "Explain Haskell monads" Nothing
-- Right "Monads in Haskell..."

-- Chat interaction
let messages = UserMessage "What's the capital of France?" :| []
chatResponse <- chat ollamaLLM messages Nothing
-- Right "The capital of France is Paris."

-- Streaming
streamHandler = StreamHandler print (putStrLn "Done")
streamResult <- stream ollamaLLM messages streamHandler Nothing
@
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

{- | Ollama LLM configuration
Contains:

- Model name (e.g., "llama3:latest")
- Callbacks for event tracking

Example:

>>> Ollama "nomic-embed" [logCallback]
Ollama "nomic-embed"
-}
data Ollama = Ollama
  { modelName :: Text
  -- ^ The name of the Ollama model
  , callbacks :: [Callback]
  -- ^ Event handlers for LLM operations
  }

instance Show Ollama where
  show (Ollama modelName _) = "Ollama " ++ show modelName

{- | Ollama implementation of the LLM typeclass
Note: Params argument is currently ignored (see TODOs).

Example instance usage:

@
-- Generate text with error handling
case generate ollamaLLM "Hello" Nothing of
  Left err -> putStrLn $ "Error: " ++ err
  Right res -> putStrLn res
@
-}
instance LLM Ollama where
  -- \| Generate text from a prompt
  --  Returns Left on API errors, Right on success.
  --
  --  Example:
  --  >>> generate (Ollama "llama3.2" []) "Hello" Nothing
  --  Right "Hello! How can I assist you today?"
  --
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

  -- \| Chat interaction with message history.
  --  Uses Ollama's chat API for multi-turn conversations.
  --
  --  Example:
  --  >>> let msgs = UserMessage "Hi" :| [AssistantMessage "Hello!"]
  --  >>> chat (Ollama "llama3" []) msgs Nothing
  --  Right "How are you today?"
  --
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

  -- \| Streaming response handling.
  --  Processes tokens in real-time via StreamHandler.
  --
  --  Example:
  --  >>> let handler = StreamHandler (putStr . ("Token: " ++)) (putStrLn "Complete")
  --  >>> stream (Ollama "llama3" []) messages handler Nothing
  --  Token: H Token: i Complete
  --
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

{- | Convert LangChain messages to Ollama format.
Current limitations:
- Ignores 'messageData' field
- No tool call support (see TODO)

Example conversion:
>>> let msg = Message System "You are an assistant" defaultMessageData
>>> toOllamaMessages (msg :| [])
NonEmpty [OllamaChat.Message System "You are an assistant" Nothing Nothing]
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
  -- \| Runnable interface implementation.
  --  Currently delegates to 'chat' method with default parameters.
  --
  invoke model input = chat model input Nothing

{- $examples
Test case patterns:
1. Basic generation
   >>> generate (Ollama "test-model" []) "Hello" Nothing
   Right "Mock response"

2. Error handling
   >>> generate (Ollama "invalid-model" []) "Test" Nothing
   Left "API request failed"

3. Streaming interaction
   >>> let handler = StreamHandler print (pure ())
   >>> stream (Ollama "llama3" []) (UserMessage "Hi" :| []) handler Nothing
   Right ()
-}
