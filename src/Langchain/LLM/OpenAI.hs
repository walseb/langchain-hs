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

This module provides the 'OpenAI' data type and implements the 'LLM' typeclass for interacting with OpenAI's language models.
It supports generating text, handling chat interactions, and streaming responses using OpenAI's API.

The 'OpenAI' type encapsulates the API key, model name, and callbacks for event handling.
The 'LLM' instance methods ('generate', 'chat', 'stream') allow for seamless integration with LangChain's processing pipelines.

For more information on OpenAI's API, see: <https://platform.openai.com/docs/api-reference>

### Example Usage

**Text Generation:**
```haskell
import Data.Text (Text)
import qualified Langchain.LLM.Core as LLM
import Langchain.LLM.OpenAI (OpenAI(..))

main :: IO ()
main = do
  let openAI = OpenAI
        { apiKey = "your-api-key"
        , openAIModelName = "gpt-3.5-turbo"
        , callbacks = []
        }
  result <- LLM.generate openAI "Tell me a joke" Nothing
  case result of
    Left err -> putStrLn $ "Error: " ++ err
    Right response -> putStrLn response
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
import qualified Langchain.LLM.Internal.OpenAI as O (OpenAIParams (..))

{- | Configuration for OpenAI's language models.

This data type holds the necessary information to interact with OpenAI's API,
including the API key, the model name, and a list of callbacks for handling events.
-}
data OpenAI = OpenAI
  { apiKey :: Text
  -- ^ The API key for authenticating with OpenAI's services.
  , openAIModelName :: Text
  -- ^ The name of the OpenAI model to use (e.g., "gpt-3.5-turbo", "gpt-4").
  , callbacks :: [Callback]
  -- ^ A list of callbacks for handling events during LLM operations.
  }

-- | Not including API key to avoid accidental leak
instance Show OpenAI where
  show OpenAI {..} = "OpenAI " ++ show openAIModelName

{- | Implementation of the 'LLM' typeclass for OpenAI models.

This instance provides methods for generating text, handling chat interactions,
and streaming responses using OpenAI's API.
-}
instance LLM.LLM OpenAI where
  type LLMParams OpenAI = OpenAIParams

  generate OpenAI {..} prompt mbOpenAIParams = do
    eRes <-
      createChatCompletion
        apiKey
        ( defaultChatCompletionRequest
            { model = openAIModelName
            , messages =
                [defaultMessage {OpenAI.content = Just (StringContent prompt)}]
            , frequencyPenalty = maybe Nothing O.frequencyPenalty mbOpenAIParams
            , logitBias = maybe Nothing O.logitBias mbOpenAIParams
            , logprobs = maybe Nothing O.logprobs mbOpenAIParams
            , maxCompletionTokens = maybe Nothing O.maxCompletionTokens mbOpenAIParams
            , maxTokens = maybe Nothing O.maxTokens mbOpenAIParams
            , metadata = maybe Nothing O.metadata mbOpenAIParams
            , modalities = maybe Nothing O.modalities mbOpenAIParams
            , n = maybe Nothing O.n mbOpenAIParams
            , parallelToolCalls = maybe Nothing O.parallelToolCalls mbOpenAIParams
            , prediction = maybe Nothing O.prediction mbOpenAIParams
            , presencePenalty = maybe Nothing O.presencePenalty mbOpenAIParams
            , reasoningEffort = maybe Nothing O.reasoningEffort mbOpenAIParams
            , responseFormat = maybe Nothing O.responseFormat mbOpenAIParams
            , seed = maybe Nothing O.seed mbOpenAIParams
            , serviceTier = maybe Nothing O.serviceTier mbOpenAIParams
            , stop = maybe Nothing O.stop mbOpenAIParams
            , store = maybe Nothing O.store mbOpenAIParams
            , temperature = maybe Nothing O.temperature mbOpenAIParams
            , toolChoice = maybe Nothing O.toolChoice mbOpenAIParams
            , tools = maybe Nothing O.tools mbOpenAIParams
            , topLogprobs = maybe Nothing O.topLogprobs mbOpenAIParams
            , topP = maybe Nothing O.topP mbOpenAIParams
            , user = maybe Nothing O.user mbOpenAIParams
            , webSearchOptions = maybe Nothing O.webSearchOptions mbOpenAIParams
            , audio = maybe Nothing O.audio mbOpenAIParams
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
  chat OpenAI {..} msgs mbOpenAIParams = do
    eRes <-
      createChatCompletion
        apiKey
        ( defaultChatCompletionRequest
            { model = openAIModelName
            , messages = toOpenAIMessages msgs
            , frequencyPenalty = maybe Nothing O.frequencyPenalty mbOpenAIParams
            , logitBias = maybe Nothing O.logitBias mbOpenAIParams
            , logprobs = maybe Nothing O.logprobs mbOpenAIParams
            , maxCompletionTokens = maybe Nothing O.maxCompletionTokens mbOpenAIParams
            , maxTokens = maybe Nothing O.maxTokens mbOpenAIParams
            , metadata = maybe Nothing O.metadata mbOpenAIParams
            , modalities = maybe Nothing O.modalities mbOpenAIParams
            , n = maybe Nothing O.n mbOpenAIParams
            , parallelToolCalls = maybe Nothing O.parallelToolCalls mbOpenAIParams
            , prediction = maybe Nothing O.prediction mbOpenAIParams
            , presencePenalty = maybe Nothing O.presencePenalty mbOpenAIParams
            , reasoningEffort = maybe Nothing O.reasoningEffort mbOpenAIParams
            , responseFormat = maybe Nothing O.responseFormat mbOpenAIParams
            , seed = maybe Nothing O.seed mbOpenAIParams
            , serviceTier = maybe Nothing O.serviceTier mbOpenAIParams
            , stop = maybe Nothing O.stop mbOpenAIParams
            , store = maybe Nothing O.store mbOpenAIParams
            , temperature = maybe Nothing O.temperature mbOpenAIParams
            , toolChoice = maybe Nothing O.toolChoice mbOpenAIParams
            , tools = maybe Nothing O.tools mbOpenAIParams
            , topLogprobs = maybe Nothing O.topLogprobs mbOpenAIParams
            , topP = maybe Nothing O.topP mbOpenAIParams
            , user = maybe Nothing O.user mbOpenAIParams
            , webSearchOptions = maybe Nothing O.webSearchOptions mbOpenAIParams
            , audio = maybe Nothing O.audio mbOpenAIParams
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

  stream OpenAI {..} msgs LLM.StreamHandler {onComplete, onToken} mbOpenAIParams = do
    let req =
          defaultChatCompletionRequest
            { model = openAIModelName
            , messages = toOpenAIMessages msgs
            , stream = Just True -- Enable streaming
            , frequencyPenalty = maybe Nothing O.frequencyPenalty mbOpenAIParams
            , logitBias = maybe Nothing O.logitBias mbOpenAIParams
            , logprobs = maybe Nothing O.logprobs mbOpenAIParams
            , maxCompletionTokens = maybe Nothing O.maxCompletionTokens mbOpenAIParams
            , maxTokens = maybe Nothing O.maxTokens mbOpenAIParams
            , metadata = maybe Nothing O.metadata mbOpenAIParams
            , modalities = maybe Nothing O.modalities mbOpenAIParams
            , n = maybe Nothing O.n mbOpenAIParams
            , parallelToolCalls = maybe Nothing O.parallelToolCalls mbOpenAIParams
            , prediction = maybe Nothing O.prediction mbOpenAIParams
            , presencePenalty = maybe Nothing O.presencePenalty mbOpenAIParams
            , reasoningEffort = maybe Nothing O.reasoningEffort mbOpenAIParams
            , responseFormat = maybe Nothing O.responseFormat mbOpenAIParams
            , seed = maybe Nothing O.seed mbOpenAIParams
            , serviceTier = maybe Nothing O.serviceTier mbOpenAIParams
            , stop = maybe Nothing O.stop mbOpenAIParams
            , store = maybe Nothing O.store mbOpenAIParams
            , temperature = maybe Nothing O.temperature mbOpenAIParams
            , toolChoice = maybe Nothing O.toolChoice mbOpenAIParams
            , tools = maybe Nothing O.tools mbOpenAIParams
            , topLogprobs = maybe Nothing O.topLogprobs mbOpenAIParams
            , topP = maybe Nothing O.topP mbOpenAIParams
            , user = maybe Nothing O.user mbOpenAIParams
            , webSearchOptions = maybe Nothing O.webSearchOptions mbOpenAIParams
            , audio = maybe Nothing O.audio mbOpenAIParams
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
