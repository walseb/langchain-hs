{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Langchain.OpenAI (
    OpenAI (..),
    Message (..),
    OpenAIReqBody (..),
) where

import Data.Aeson
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics
import Langchain.LLM
import Network.HTTP.Simple
import System.Environment (lookupEnv)

newtype OpenAI = OpenAI {mbApiKey :: Maybe Text}

data OpenAIReqBody = OpenAIReqBody
    { model :: Text
    , messages :: NonEmpty Message
    , temperature :: Maybe Double
    , max_tokens :: Maybe Integer
    , top_p :: Maybe Double
    , n :: Maybe Int
    , stop :: Maybe [Text]
    , stream :: Maybe Bool
    }
    deriving (Eq, Show, Generic)

instance ToJSON OpenAIReqBody where
    toJSON = genericToJSON defaultOptions{omitNothingFields = True}

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
    call (OpenAI mbKey) prompt mbParams = do
        let messages = Message User prompt Nothing NE.:| []
        chat (OpenAI mbKey) messages mbParams

    chat (OpenAI mbKey) messages mbParams = do
        apiKey <- case mbKey of
            Nothing -> do
                mKey <- lookupEnv "OPENAI_API_KEY"
                case mKey of
                    Nothing -> error "API key not found"
                    Just key -> pure $ T.pack key
            Just key -> pure key
        initRequest <- parseRequest "https://api.openai.com/v1/chat/completions"
        let reqBody =
                OpenAIReqBody
                    { model = "gpt-4o-mini-2024-07-18"
                    , messages = messages
                    , temperature = (\Params{..} -> temperature) =<< mbParams
                    , max_tokens = maxTokens =<< mbParams
                    , top_p = (\Params{..} -> topP) =<< mbParams
                    , n = (\Params{..} -> n) =<< mbParams
                    , stop = (\Params{..} -> stop) =<< mbParams
                    , stream = Nothing
                    }
        let request =
                setRequestMethod "POST"
                    $ setRequestBodyJSON reqBody
                    $ setRequestHeaders
                        [ ("Authorization", "Bearer " <> TE.encodeUtf8 apiKey)
                        , ("Content-Type", "application/json")
                        ]
                     initRequest
        resp <- httpLbs request
        let statusCode = getResponseStatusCode resp
        if statusCode >= 200 && statusCode <= 299
            then do
                let respBody = getResponseBody resp
                case eitherDecode respBody of
                    Left err -> error $ "Decoding error: " <> err
                    Right res ->
                        case choices res of
                            [] -> error "No choices in response"
                            (choice : _) -> pure $ (\Message_{..} -> content) $ message choice
            else error $ "HTTP error: " <> show statusCode <> " - " <> show (getResponseBody resp)
