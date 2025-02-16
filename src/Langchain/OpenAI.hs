{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Langchain.OpenAI (
    OpenAI (..),
    Message (..),
    OpenAIReqBody (..),
) where

import Control.Monad (void)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics
import Langchain.LLM
import Network.HTTP.Simple
import System.Environment (lookupEnv)
import Data.List.NonEmpty (NonEmpty)

newtype OpenAI = OpenAI {mbApiKey :: Maybe Text}

data OpenAIReqBody = OpenAIReqBody
    { model :: Text
    , messages :: NonEmpty Message
    }
    deriving (Eq, Show, Generic, ToJSON)

-- Define the data types based on the JSON structure
data OpenAIResponse = OpenAIResponse
    { id :: Text
    , object :: Text
    , created :: Integer
    , model :: Text
    , systemFingerprint :: Maybe Text
    , choices :: [Choice]
    , serviceTier :: Text
    , usage :: Usage
    }
    deriving (Show, Generic)

instance FromJSON OpenAIResponse
instance ToJSON OpenAIResponse

data Choice = Choice
    { index :: Int
    , message :: Message_
    , logprobs :: Maybe Logprobs
    , finishReason :: Text
    }
    deriving (Show, Generic)

instance FromJSON Choice
instance ToJSON Choice

data Message_ = Message_
    { role :: Text
    , content :: Text
    }
    deriving (Show, Generic)

instance FromJSON Message_
instance ToJSON Message_

data Logprobs = Logprobs
    {
    }
    -- Define fields if needed
    deriving (Show, Generic)

instance FromJSON Logprobs
instance ToJSON Logprobs

data Usage = Usage
    { promptTokens :: Int
    , completionTokens :: Int
    , totalTokens :: Int
    , completionTokensDetails :: CompletionTokensDetails
    }
    deriving (Show, Generic)

instance FromJSON Usage
instance ToJSON Usage

data CompletionTokensDetails = CompletionTokensDetails
    { reasoningTokens :: Int
    , acceptedPredictionTokens :: Int
    , rejectedPredictionTokens :: Int
    }
    deriving (Show, Generic)

instance FromJSON CompletionTokensDetails
instance ToJSON CompletionTokensDetails

instance LLM OpenAI where
    chat (OpenAI mbKey) messages _ = do
        apiKey <- case mbKey of
            Nothing -> do
                mKey <- lookupEnv "OPENAI_API_KEY"
                case mKey of
                    Nothing -> error "api Key not found"
                    Just apiKey -> return $ T.pack apiKey
            Just apiKey -> pure apiKey
        initRequest <- parseRequest "https://api.openai.com/v1/chat/completions"
        let reqBody = OpenAIReqBody "gpt-4o-mini-2024-07-18" messages
        let request =
                setRequestMethod
                    "POST"
                    ( setRequestBodyJSON
                        reqBody
                        ( setRequestHeaders
                            [
                                ( "Authorization"
                                , "Bearer "
                                    <> TE.encodeUtf8 apiKey
                                )
                            ]
                            initRequest
                        )
                    )
        resp <- httpLbs request
        let statusCode = getResponseStatusCode resp
        if statusCode >= 200 && statusCode <= 299
            then do
                let respBody = getResponseBody resp
                case eitherDecodeStrict (BSL.toStrict respBody) :: Either String OpenAIResponse of
                    Left err -> do
                        void $ error (show err)
                        pure "Error"
                    Right res -> do
                        let Choice{message = Message_{content}} = head (choices res)
                        pure content
            else error $ "Something went wrong: " <> show (getResponseBody resp)
