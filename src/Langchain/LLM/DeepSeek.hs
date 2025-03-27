module Langchain.LLM.DeepSeek () where
{-
(
    DeepSeek (..),
    DeepSeekReqBody (..),
    listDeepseekModels,
) where

import Control.Monad (void)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics
import Langchain.LLM
import Network.HTTP.Simple
import Prelude hiding (id)

-- DeepSeek instance definition
data DeepSeek = DeepSeek
    { apiKey :: Text
    , modelName :: Text
    }

instance Show DeepSeek where
    show (DeepSeek _ mName) = "api Key: xxxxx, modelName " <> T.unpack mName

-- Updated DeepSeekReqBody
data DeepSeekReqBody = DeepSeekReqBody
    { messages :: NonEmpty Message
    , model :: Text
    , frequencyPenalty :: Maybe Int
    , maxTokens :: Maybe Int
    , presencePenalty :: Maybe Int
    , responseFormat :: Maybe Object
    , stop :: Maybe [Text]
    , stream :: Maybe Bool
    , temperature :: Maybe Double
    , logprobs :: Maybe Bool
    }
    deriving (Eq, Show)

instance ToJSON DeepSeekReqBody where
    toJSON DeepSeekReqBody{..} =
        object
            [ ("messages", toJSON messages)
            , ("model", String model)
            , ("frequency_penalty", maybe Null (Number . fromIntegral) frequencyPenalty)
            , ("max_tokens", maybe Null (Number . fromIntegral) maxTokens)
            , ("presence_penalty", maybe Null (Number . fromIntegral) presencePenalty)
            , ("response_format", maybe Null toJSON responseFormat)
            , ("stop", maybe Null (toJSON . map String) stop)
            , ("stream", maybe Null Bool stream)
            , ("temperature", maybe Null (Number . realToFrac) temperature)
            , ("logprobs", maybe Null Bool logprobs)
            ]

-- Existing response types (unchanged)
data FinishReason = Stop | Length | ContentFilter | ToolCalls | InsufficientSystemResource
    deriving (Show, Eq)

instance FromJSON FinishReason where
    parseJSON (String "stop") = return Stop
    parseJSON (String "length") = return Length
    parseJSON (String "content_filter") = return ContentFilter
    parseJSON (String "tool_calls") = return ToolCalls
    parseJSON (String "insufficient_system_resource") = return InsufficientSystemResource
    parseJSON x = error $ "Cannot parse json for Finish_reason " <> show x

data Choice = Choice
    { finishReason :: FinishReason
    , index :: Int
    , message :: Message
    , logprobs :: Maybe Object
    }
    deriving (Show, Eq)

instance FromJSON Choice where
    parseJSON (Object v) =
        Choice
            <$> v .: "finish_reason"
            <*> v .: "index"
            <*> v .: "message"
            <*> v .: "logprobs"
    parseJSON _ = error "cannot parse given json for choice"

data Usage = Usage
    { completionTokens :: Integer
    , promptTokens :: Integer
    , promptCacheHitTokens :: Integer
    , promptCacheMissTokens :: Integer
    , totalTokens :: Integer
    , completionTokensDetails :: Object
    }
    deriving (Show, Eq)

instance FromJSON Usage where
    parseJSON (Object v) =
        Usage
            <$> v .: "completion_tokens"
            <*> v .: "prompt_tokens"
            <*> v .: "prompt_cache_hit_tokens"
            <*> v .: "prompt_cache_miss_tokens"
            <*> v .: "total_tokens"
            <*> v .: "completion_tokens_details"
    parseJSON _ = error "cannot parse given json for Usage"

data DeepSeekResp = DeepSeekResp
    { id :: String
    , choices :: [Choice]
    , created :: Integer
    , model :: String
    , system_fingerprint :: String
    , usage :: Usage
    }
    deriving (Eq, Show, Generic, FromJSON)

-- LLM instance implementation
instance LLM DeepSeek where
    call (DeepSeek apiKey modelName) prompt mbParams = do
        let messages = Message User prompt Nothing :| []
        chat (DeepSeek apiKey modelName) messages mbParams

    chat (DeepSeek apiKey modelName) messages mbParams = do
        initRequest <- parseRequest "https://api.deepseek.com/chat/completions"
        let reqBody = DeepSeekReqBody
                { messages = messages
                , model = modelName
                , frequencyPenalty = Nothing
                , maxTokens = fmap fromIntegral ((\Params{..} -> maxTokens) =<< mbParams)
                , presencePenalty = Nothing
                , responseFormat = Nothing
                , stop = (\Params{..} -> stop) =<< mbParams
                , stream = Nothing
                , temperature = (\Params{..} -> temperature) =<< mbParams
                , logprobs = Nothing
                }
        let request = setRequestMethod "POST"
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
                case eitherDecodeStrict (BSL.toStrict respBody) :: Either String DeepSeekResp of
                    Left err -> error $ "Decoding error: " <> err
                    Right DeepSeekResp{..} ->
                        case choices of
                            [] -> error "No choices in response"
                            (choice:_) -> pure $ content (message choice)
            else error $ "HTTP error: " <> show statusCode <> " - " <> show (getResponseBody resp)

-- Existing listDeepseekModels function (unchanged)
data ListDeepSeekModels = ListDeepSeekModels
    { id :: Text
    , owned_by :: String
    }
    deriving (Show, Eq, Generic, FromJSON)

listDeepseekModels :: IO [Text]
listDeepseekModels = do
    initRequest <- parseRequest "https://api.deepseek.com/models"
    let request = setRequestMethod "GET" initRequest
    resp <- httpLbs request
    let statusCode = getResponseStatusCode resp
    if statusCode >= 200 && statusCode <= 299
        then do
            let respBody = getResponseBody resp
            case eitherDecodeStrict (BSL.toStrict respBody) :: Either String [ListDeepSeekModels] of
                Left err -> do
                    void $ error (show err)
                    pure []
                Right res -> pure $ map (\ListDeepSeekModels{id = id} -> id) res
        else error $ "Something went wrong: " <> show (getResponseBody resp)
        -}
