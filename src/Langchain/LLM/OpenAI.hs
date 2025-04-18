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

OpenAI implementation of LangChain's LLM interface.
-}
module Langchain.LLM.OpenAI
  ( OpenAI (..)

    -- * Functions
  , createChatCompletion
  ) where

import Conduit
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import qualified Data.List.NonEmpty as NE
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Langchain.Callback (Callback)
import qualified Langchain.LLM.Core as LLM
import Langchain.LLM.OpenAI.Stream
import Langchain.LLM.OpenAI.Types
import Network.HTTP.Conduit
import Network.HTTP.Simple
import Network.HTTP.Types.Status (statusCode)

{- | Creates a chat completion request
Sends the request to OpenAI API and returns the parsed response.

Example usage:
@
response <- createChatCompletion "your-api-key" request
case response of
  Right res -> print (choices res)
  Left err -> putStrLn err
@
-}
createChatCompletion :: Text -> ChatCompletionRequest -> IO (Either String ChatCompletionResponse)
createChatCompletion apiKey r = do
  request_ <- parseRequest "https://api.openai.com/v1/chat/completions"
  let req =
        setRequestMethod "POST" $
          setRequestSecure True $
            setRequestHost "api.openai.com" $
              setRequestPath "/v1/chat/completions" $
                setRequestHeader "Content-Type" ["application/json"] $
                  setRequestHeader "Authorization" ["Bearer " <> encodeUtf8 apiKey] $
                    setRequestBodyJSON r $
                      request_

  response <- httpLBS req
  let status = statusCode $ getResponseStatus response
  if status >= 200 && status < 300
    then case eitherDecode (getResponseBody response) of
      Left err -> return $ Left $ "JSON parse error: " <> err
      Right completionResponse -> return $ Right completionResponse
    else return $ Left $ "API error: " <> show status <> " " <> show (getResponseBody response)

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
                [ Message
                    { role = User
                    , content = Just (StringContent prompt)
                    , name = Nothing
                    , functionCall = Nothing
                    , toolCalls = Nothing
                    , toolCallId = Nothing
                    , audio = Nothing
                    , refusal = Nothing
                    }
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
    request_ <- parseRequest "POST https://api.openai.com/v1/chat/completions"
    let httpReq =
          setRequestHeader "Authorization" ["Bearer " <> encodeUtf8 apiKey] $
            setRequestHeader "Content-Type" ["application/json"] $
              setRequestBodyJSON req $
                request_

    manager <- newManager tlsManagerSettings
    runResourceT $ do
      response <- http httpReq manager
      bufferRef <- liftIO $ newIORef BS.empty
      runConduit $
        responseBody response
          .| linesUnboundedAsciiC
          .| mapM_C (liftIO . processLine bufferRef)

    onComplete
    return $ Right ()
    where
      processLine bufferRef line = do
        if BS.isPrefixOf "data: " line
          then do
            if line == "data: [DONE]"
              then return () -- Stream is complete
              else do
                let content = BS.drop 6 line -- Remove "data: " prefix
                case decode (LBS.fromStrict content) of
                  Just chunk -> handleChunk chunk
                  Nothing -> do
                    -- Handle potential partial JSON by buffering
                    oldBuffer <- readIORef bufferRef
                    let newBuffer = oldBuffer <> content
                    writeIORef bufferRef newBuffer
                    -- Try to parse the combined buffer
                    case decode (LBS.fromStrict newBuffer) of
                      Just chunk -> do
                        handleChunk chunk
                        writeIORef bufferRef BS.empty -- Clear buffer after successful parse
                      Nothing -> return () -- Keep in buffer for next chunk
          else return () -- Ignore non-data lines
      handleChunk :: ChatCompletionChunk -> IO ()
      handleChunk ChatCompletionChunk {..} = do
        case listToMaybe chunkChoices of
          Nothing -> pure ()
          Just ChunkChoice {..} -> maybe (pure ()) onToken ((\Delta {..} -> content) delta)

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
