{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Langchain.LLM.Internal.OpenAI
  ( ChatCompletionChunk (..)
  , ChunkChoice (..)
  , Delta (..)
  , OpenAIStreamHandler (..)
  , ChatCompletionRequest (..)
  , ChatCompletionResponse (..)
  , Message (..)
  , Role (..)
  , MessageContent (..)
  , TextContent (..)
  , Tool_ (..)
  , Function_ (..)
  , ToolCall (..)
  , FunctionCall_ (..)
  , Usage (..)
  , Choice (..)
  , FinishReason (..)
  , LogProbs (..)
  , LogProbContent (..)
  , TopLogProb (..)
  , AudioConfig (..)
  , AudioResponse (..)
  , Modality (..)
  , ToolChoice (..)
  , SpecificToolChoice (..)
  , ReasoningEffort (..)
  , PredictionOutput (..)
  , PredictionContent (..)
  , ResponseFormat (..)
  , StreamOptions (..)
  , WebSearchOptions (..)
  , UserLocation (..)
  , ApproximateLocation (..)
  , CompletionTokensDetails (..)
  , PromptTokensDetails (..)
  , defaultChatCompletionRequest
  , createChatCompletion
  , createChatCompletionStream
  , defaultMessage
  ) where

import Conduit
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import Data.Map (Map)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics
import Network.HTTP.Conduit
import Network.HTTP.Simple
import Network.HTTP.Types.Status (statusCode)

data ChatCompletionChunk = ChatCompletionChunk
  { chunkChoices :: [ChunkChoice]
  }
  deriving (Show)

instance FromJSON ChatCompletionChunk where
  parseJSON = withObject "ChatCompletionChunk" $ \v ->
    ChatCompletionChunk <$> v .: "choices"

data ChunkChoice = ChunkChoice
  { delta :: Delta
  , finishReason :: Maybe FinishReason
  }
  deriving (Show)

instance FromJSON ChunkChoice where
  parseJSON = withObject "ChunkChoice" $ \v ->
    ChunkChoice <$> v .: "delta" <*> v .:? "finish_reason"

data Delta = Delta
  { content :: Maybe Text
  }
  deriving (Show)

instance FromJSON Delta where
  parseJSON = withObject "Delta" $ \v ->
    Delta <$> v .:? "content"

{- | Represents different roles in a conversation
User: Human user input
Assistant: AI-generated response
System: System-level instructions
Developer: Special role for developer messages
Tool: Tool interaction messages
Function: Function call messages
-}
data Role
  = User
  | Assistant
  | System
  | Developer
  | Tool
  | Function
  deriving (Show, Eq, Generic)

instance ToJSON Role where
  toJSON User = String "user"
  toJSON Assistant = String "assistant"
  toJSON System = String "system"
  toJSON Developer = String "developer"
  toJSON Tool = String "tool"
  toJSON Function = String "function"

instance FromJSON Role where
  parseJSON (String "user") = return User
  parseJSON (String "assistant") = return Assistant
  parseJSON (String "system") = return System
  parseJSON (String "developer") = return Developer
  parseJSON (String "tool") = return Tool
  parseJSON (String "function") = return Function
  parseJSON invalid = fail $ "Invalid role: " ++ show invalid

data TextContent = TextContent
  { text_ :: Text
  , contentType :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON TextContent where
  toJSON TextContent {..} =
    object
      [ "text" .= text_
      , "type" .= contentType
      ]

instance FromJSON TextContent where
  parseJSON = withObject "TextContent" $ \v ->
    TextContent
      <$> v .: "text"
      <*> v .: "type"

data MessageContent
  = StringContent Text
  | ContentParts [TextContent]
  deriving (Show, Eq, Generic)

instance ToJSON MessageContent where
  toJSON (StringContent text) = String text
  toJSON (ContentParts parts) = toJSON parts

instance FromJSON MessageContent where
  parseJSON (String s) = return $ StringContent s
  parseJSON (Array arr) = ContentParts <$> parseJSON (Array arr)
  parseJSON invalid = fail $ "Invalid message content: " ++ show invalid

data Function_ = Function_
  { name :: Text
  , description :: Maybe Text
  , parameters :: Maybe Value
  , strict :: Maybe Bool
  }
  deriving (Show, Eq, Generic)

instance ToJSON Function_ where
  toJSON Function_ {..} =
    object $
      [ "name" .= name
      ]
        ++ maybe [] (\d -> ["description" .= d]) description
        ++ maybe [] (\p -> ["parameters" .= p]) parameters
        ++ maybe [] (\s -> ["strict" .= s]) strict

instance FromJSON Function_ where
  parseJSON = withObject "Function" $ \v ->
    Function_
      <$> v .: "name"
      <*> v .:? "description"
      <*> v .:? "parameters"
      <*> v .:? "strict"

data Tool_ = Tool_
  { toolType :: Text
  , function :: Function_
  }
  deriving (Show, Eq, Generic)

instance ToJSON Tool_ where
  toJSON Tool_ {..} =
    object
      [ "type" .= toolType
      , "function" .= function
      ]

instance FromJSON Tool_ where
  parseJSON = withObject "Tool" $ \v ->
    Tool_
      <$> v .: "type"
      <*> v .: "function"

data FunctionCall_ = FunctionCall_
  { name :: Text
  , arguments :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON FunctionCall_ where
  toJSON FunctionCall_ {..} =
    object
      [ "name" .= name
      , "arguments" .= arguments
      ]

instance FromJSON FunctionCall_ where
  parseJSON = withObject "FunctionCall" $ \v ->
    FunctionCall_
      <$> v .: "name"
      <*> v .: "arguments"

data ToolCall = ToolCall
  { id_ :: Text
  , toolType :: Text
  , function :: FunctionCall_
  }
  deriving (Show, Eq, Generic)

instance ToJSON ToolCall where
  toJSON ToolCall {..} =
    object
      [ "id" .= id_
      , "type" .= toolType
      , "function" .= function
      ]

instance FromJSON ToolCall where
  parseJSON = withObject "ToolCall" $ \v ->
    ToolCall
      <$> v .: "id"
      <*> v .: "type"
      <*> v .: "function"

{- | Configuration for audio processing
Specifies format and voice preferences for text-to-speech
-}
data AudioConfig = AudioConfig
  { format :: Text
  , voice :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON AudioConfig where
  toJSON AudioConfig {..} =
    object
      [ "format" .= format
      , "voice" .= voice
      ]

instance FromJSON AudioConfig where
  parseJSON = withObject "AudioConfig" $ \v ->
    AudioConfig
      <$> v .: "format"
      <*> v .: "voice"

data AudioResponse = AudioResponse
  { data_ :: Text
  , expiresAt :: Integer
  , id_ :: Text
  , transcript :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON AudioResponse where
  toJSON AudioResponse {..} =
    object
      [ "data" .= data_
      , "expires_at" .= expiresAt
      , "id" .= id_
      , "transcript" .= transcript
      ]

instance FromJSON AudioResponse where
  parseJSON = withObject "AudioResponse" $ \v ->
    AudioResponse
      <$> v .: "data"
      <*> v .: "expires_at"
      <*> v .: "id"
      <*> v .: "transcript"

{- | Represents a single message in a conversation
Contains role, content, and optional metadata like function calls or audio responses.
-}
data Message = Message
  { role :: Role
  , content :: Maybe MessageContent
  , name :: Maybe Text
  , functionCall :: Maybe FunctionCall_
  , toolCalls :: Maybe [ToolCall]
  , toolCallId :: Maybe Text
  , audio :: Maybe AudioResponse
  , refusal :: Maybe Text
  }
  deriving (Show, Eq, Generic)

defaultMessage :: Message
defaultMessage =
  Message
    { role = User
    , content = Nothing
    , name = Nothing
    , functionCall = Nothing
    , toolCalls = Nothing
    , toolCallId = Nothing
    , audio = Nothing
    , refusal = Nothing
    }

instance ToJSON Message where
  toJSON Message {..} =
    object $
      ["role" .= role]
        ++ maybe [] (\c -> ["content" .= c]) content
        ++ maybe [] (\n -> ["name" .= n]) name
        ++ maybe [] (\fc -> ["function_call" .= fc]) functionCall
        ++ maybe [] (\tc -> ["tool_calls" .= tc]) toolCalls
        ++ maybe [] (\tcid -> ["tool_call_id" .= tcid]) toolCallId
        ++ maybe [] (\a -> ["audio" .= a]) audio
        ++ maybe [] (\r -> ["refusal" .= r]) refusal

instance FromJSON Message where
  parseJSON = withObject "Message" $ \v ->
    Message
      <$> v .: "role"
      <*> v .:? "content"
      <*> v .:? "name"
      <*> v .:? "function_call"
      <*> v .:? "tool_calls"
      <*> v .:? "tool_call_id"
      <*> v .:? "audio"
      <*> v .:? "refusal"

data Modality = TextModality | AudioModality
  deriving (Show, Eq, Generic)

instance ToJSON Modality where
  toJSON TextModality = String "text"
  toJSON AudioModality = String "audio"

instance FromJSON Modality where
  parseJSON (String "text") = return TextModality
  parseJSON (String "audio") = return AudioModality
  parseJSON invalid = fail $ "Invalid modality: " ++ show invalid

data ToolChoice = None | Auto | Required | SpecificTool SpecificToolChoice
  deriving (Show, Eq, Generic)

instance ToJSON ToolChoice where
  toJSON None = String "none"
  toJSON Auto = String "auto"
  toJSON Required = String "required"
  toJSON (SpecificTool choice) = toJSON choice

instance FromJSON ToolChoice where
  parseJSON (String "none") = return None
  parseJSON (String "auto") = return Auto
  parseJSON (String "required") = return Required
  parseJSON o@(Object _) = SpecificTool <$> parseJSON o
  parseJSON invalid = fail $ "Invalid tool choice: " ++ show invalid

data SpecificToolChoice = SpecificToolChoice
  { toolType :: Text
  , function :: Value
  }
  deriving (Show, Eq, Generic)

instance ToJSON SpecificToolChoice where
  toJSON SpecificToolChoice {..} =
    object
      [ "type" .= toolType
      , "function" .= function
      ]

instance FromJSON SpecificToolChoice where
  parseJSON = withObject "SpecificToolChoice" $ \v ->
    SpecificToolChoice
      <$> v .: "type"
      <*> v .: "function"

data ReasoningEffort = Low | Medium | High
  deriving (Show, Eq, Generic)

instance ToJSON ReasoningEffort where
  toJSON Low = String "low"
  toJSON Medium = String "medium"
  toJSON High = String "high"

instance FromJSON ReasoningEffort where
  parseJSON (String "low") = return Low
  parseJSON (String "medium") = return Medium
  parseJSON (String "high") = return High
  parseJSON invalid = fail $ "Invalid reasoning effort: " ++ show invalid

data PredictionContent = PredictionContent
  { content :: MessageContent
  , contentType :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON PredictionContent where
  toJSON PredictionContent {..} =
    object
      [ "content" .= content
      , "type" .= contentType
      ]

instance FromJSON PredictionContent where
  parseJSON = withObject "PredictionContent" $ \v ->
    PredictionContent
      <$> v .: "content"
      <*> v .: "type"

data PredictionOutput = PredictionOutput
  { predictionType :: Text
  , content :: MessageContent
  }
  deriving (Show, Eq, Generic)

instance ToJSON PredictionOutput where
  toJSON PredictionOutput {..} =
    object
      [ "type" .= predictionType
      , "content" .= content
      ]

instance FromJSON PredictionOutput where
  parseJSON = withObject "PredictionOutput" $ \v ->
    PredictionOutput
      <$> v .: "type"
      <*> v .: "content"

data ResponseFormat = JsonObjectFormat | JsonSchemaFormat Value
  deriving (Show, Eq, Generic)

instance ToJSON ResponseFormat where
  toJSON JsonObjectFormat = object ["type" .= ("json_object" :: Text)]
  toJSON (JsonSchemaFormat schema) =
    object
      [ "type" .= ("json_schema" :: Text)
      , "json_schema" .= schema
      ]

instance FromJSON ResponseFormat where
  parseJSON = withObject "ResponseFormat" $ \v -> do
    formatType <- v .: "type"
    case formatType of
      String "json_object" -> return JsonObjectFormat
      String "json_schema" -> JsonSchemaFormat <$> v .: "json_schema"
      _ -> fail $ "Invalid response format type: " ++ show formatType

data StreamOptions = StreamOptions
  { includeUsage :: Bool
  }
  deriving (Show, Eq, Generic)

instance ToJSON StreamOptions where
  toJSON StreamOptions {..} =
    object
      [ "include_usage" .= includeUsage
      ]

instance FromJSON StreamOptions where
  parseJSON = withObject "StreamOptions" $ \v ->
    StreamOptions <$> v .: "include_usage"

data ApproximateLocation = ApproximateLocation
  { locationType :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON ApproximateLocation where
  toJSON ApproximateLocation {..} =
    object
      [ "type" .= locationType
      ]

instance FromJSON ApproximateLocation where
  parseJSON = withObject "ApproximateLocation" $ \v ->
    ApproximateLocation <$> v .: "type"

data UserLocation = UserLocation
  { approximate :: ApproximateLocation
  }
  deriving (Show, Eq, Generic)

instance ToJSON UserLocation where
  toJSON UserLocation {..} =
    object
      [ "approximate" .= approximate
      ]

instance FromJSON UserLocation where
  parseJSON = withObject "UserLocation" $ \v ->
    UserLocation <$> v .: "approximate"

data WebSearchOptions = WebSearchOptions
  { searchContextSize :: Maybe Text
  , userLocation :: Maybe UserLocation
  }
  deriving (Show, Eq, Generic)

instance ToJSON WebSearchOptions where
  toJSON WebSearchOptions {..} =
    object $
      maybe [] (\s -> ["search_context_size" .= s]) searchContextSize
        ++ maybe [] (\l -> ["user_location" .= l]) userLocation

instance FromJSON WebSearchOptions where
  parseJSON = withObject "WebSearchOptions" $ \v ->
    WebSearchOptions
      <$> v .:? "search_context_size"
      <*> v .:? "user_location"

{- | Main request type for chat completions
Contains all parameters for configuring the OpenAI chat completion API call.
-}
data ChatCompletionRequest = ChatCompletionRequest
  { messages :: [Message]
  , model :: Text
  , frequencyPenalty :: Maybe Double
  , logitBias :: Maybe (Map Text Double)
  , logprobs :: Maybe Bool
  , maxCompletionTokens :: Maybe Int
  , maxTokens :: Maybe Int
  , metadata :: Maybe (Map Text Text)
  , modalities :: Maybe [Modality]
  , n :: Maybe Int
  , parallelToolCalls :: Maybe Bool
  , prediction :: Maybe PredictionOutput
  , presencePenalty :: Maybe Double
  , reasoningEffort :: Maybe ReasoningEffort
  , responseFormat :: Maybe ResponseFormat
  , seed :: Maybe Int
  , serviceTier :: Maybe Text
  , stop :: Maybe (Either Text [Text])
  , store :: Maybe Bool
  , stream :: Maybe Bool
  , streamOptions :: Maybe StreamOptions
  , temperature :: Maybe Double
  , toolChoice :: Maybe ToolChoice
  , tools :: Maybe [Tool_]
  , topLogprobs :: Maybe Int
  , topP :: Maybe Double
  , user :: Maybe Text
  , webSearchOptions :: Maybe WebSearchOptions
  , audio :: Maybe AudioConfig
  }
  deriving (Show, Eq, Generic)

instance ToJSON ChatCompletionRequest where
  toJSON ChatCompletionRequest {..} =
    object $
      [ "messages" .= messages
      , "model" .= model
      ]
        ++ maybe [] (\fp -> ["frequency_penalty" .= fp]) frequencyPenalty
        ++ maybe [] (\lb -> ["logit_bias" .= lb]) logitBias
        ++ maybe [] (\lp -> ["logprobs" .= lp]) logprobs
        ++ maybe [] (\mt -> ["max_completion_tokens" .= mt]) maxCompletionTokens
        ++ maybe [] (\mt -> ["max_tokens" .= mt]) maxTokens
        ++ maybe [] (\md -> ["metadata" .= md]) metadata
        ++ maybe [] (\m -> ["modalities" .= m]) modalities
        ++ maybe [] (\n' -> ["n" .= n']) n
        ++ maybe [] (\ptc -> ["parallel_tool_calls" .= ptc]) parallelToolCalls
        ++ maybe [] (\p -> ["prediction" .= p]) prediction
        ++ maybe [] (\pp -> ["presence_penalty" .= pp]) presencePenalty
        ++ maybe [] (\re -> ["reasoning_effort" .= re]) reasoningEffort
        ++ maybe [] (\rf -> ["response_format" .= rf]) responseFormat
        ++ maybe [] (\s -> ["seed" .= s]) seed
        ++ maybe [] (\st -> ["service_tier" .= st]) serviceTier
        ++ maybe [] (\s -> ["stop" .= s]) stop
        ++ maybe [] (\s -> ["store" .= s]) store
        ++ maybe [] (\s -> ["stream" .= s]) stream
        ++ maybe [] (\so -> ["stream_options" .= so]) streamOptions
        ++ maybe [] (\t -> ["temperature" .= t]) temperature
        ++ maybe [] (\tc -> ["tool_choice" .= tc]) toolChoice
        ++ maybe [] (\t -> ["tools" .= t]) tools
        ++ maybe [] (\tlp -> ["top_logprobs" .= tlp]) topLogprobs
        ++ maybe [] (\tp -> ["top_p" .= tp]) topP
        ++ maybe [] (\u -> ["user" .= u]) user
        ++ maybe [] (\wso -> ["web_search_options" .= wso]) webSearchOptions
        ++ maybe [] (\a -> ["audio" .= a]) audio

-- Response Types
data FinishReason = Stop | Length | ContentFilter | ToolCalls | FunctionCall
  deriving (Show, Eq, Generic)

instance FromJSON FinishReason where
  parseJSON (String "stop") = return Stop
  parseJSON (String "length") = return Length
  parseJSON (String "content_filter") = return ContentFilter
  parseJSON (String "tool_calls") = return ToolCalls
  parseJSON (String "function_call") = return FunctionCall
  parseJSON invalid = fail $ "Invalid finish reason: " ++ show invalid

data TopLogProb = TopLogProb
  { bytes :: Maybe [Int]
  , logprob :: Double
  , token :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON TopLogProb where
  parseJSON = withObject "TopLogProb" $ \v ->
    TopLogProb
      <$> v .:? "bytes"
      <*> v .: "logprob"
      <*> v .: "token"

data LogProbContent = LogProbContent
  { bytes :: Maybe [Int]
  , logprob :: Double
  , token :: Text
  , topLogprobs :: [TopLogProb]
  }
  deriving (Show, Eq, Generic)

instance FromJSON LogProbContent where
  parseJSON = withObject "LogProbContent" $ \v ->
    LogProbContent
      <$> v .:? "bytes"
      <*> v .: "logprob"
      <*> v .: "token"
      <*> v .: "top_logprobs"

data LogProbs = LogProbs
  { content :: Maybe [LogProbContent]
  , refusal :: Maybe [LogProbContent]
  }
  deriving (Show, Eq, Generic)

instance FromJSON LogProbs where
  parseJSON = withObject "LogProbs" $ \v ->
    LogProbs
      <$> v .:? "content"
      <*> v .:? "refusal"

data CompletionTokensDetails = CompletionTokensDetails
  { acceptedPredictionTokens :: Int
  , audioTokens :: Int
  , reasoningTokens :: Int
  , rejectedPredictionTokens :: Int
  }
  deriving (Show, Eq, Generic)

instance FromJSON CompletionTokensDetails where
  parseJSON = withObject "CompletionTokensDetails" $ \v ->
    CompletionTokensDetails
      <$> v .: "accepted_prediction_tokens"
      <*> v .: "audio_tokens"
      <*> v .: "reasoning_tokens"
      <*> v .: "rejected_prediction_tokens"

data PromptTokensDetails = PromptTokensDetails
  { audioTokens :: Int
  , cachedTokens :: Int
  }
  deriving (Show, Eq, Generic)

instance FromJSON PromptTokensDetails where
  parseJSON = withObject "PromptTokensDetails" $ \v ->
    PromptTokensDetails
      <$> v .: "audio_tokens"
      <*> v .: "cached_tokens"

data Usage = Usage
  { completionTokens :: Int
  , promptTokens :: Int
  , totalTokens :: Int
  , completionTokensDetails :: Maybe CompletionTokensDetails
  , promptTokensDetails :: Maybe PromptTokensDetails
  }
  deriving (Show, Eq, Generic)

instance FromJSON Usage where
  parseJSON = withObject "Usage" $ \v ->
    Usage
      <$> v .: "completion_tokens"
      <*> v .: "prompt_tokens"
      <*> v .: "total_tokens"
      <*> v .:? "completion_tokens_details"
      <*> v .:? "prompt_tokens_details"

data Choice = Choice
  { finishReason :: Maybe FinishReason
  , index :: Int
  , logprobs :: Maybe LogProbs
  , message :: Message
  }
  deriving (Show, Eq, Generic)

instance FromJSON Choice where
  parseJSON = withObject "Choice" $ \v ->
    Choice
      <$> v .: "finish_reason"
      <*> v .: "index"
      <*> v .:? "logprobs"
      <*> v .: "message"

data ChatCompletionResponse = ChatCompletionResponse
  { choices :: [Choice]
  , created :: Integer
  , id_ :: Text
  , responseModel :: Text
  , object_ :: Text
  , serviceTier :: Maybe Text
  , systemFingerprint :: Text
  , usage :: Usage
  }
  deriving (Show, Eq, Generic)

instance FromJSON ChatCompletionResponse where
  parseJSON = withObject "ChatCompletionResponse" $ \v ->
    ChatCompletionResponse
      <$> v .: "choices"
      <*> v .: "created"
      <*> v .: "id"
      <*> v .: "model"
      <*> v .: "object"
      <*> v .:? "service_tier"
      <*> v .: "system_fingerprint"
      <*> v .: "usage"

{- | Default chat completion request
Uses "gpt-4.1-nano" as the default model. All other parameters are set to Nothing.
-}
defaultChatCompletionRequest :: ChatCompletionRequest
defaultChatCompletionRequest =
  ChatCompletionRequest
    { messages = []
    , model = "gpt-4.1-nano"
    , frequencyPenalty = Nothing
    , logitBias = Nothing
    , logprobs = Nothing
    , maxCompletionTokens = Nothing
    , maxTokens = Nothing
    , metadata = Nothing
    , modalities = Nothing
    , n = Nothing
    , parallelToolCalls = Nothing
    , prediction = Nothing
    , presencePenalty = Nothing
    , reasoningEffort = Nothing
    , responseFormat = Nothing
    , seed = Nothing
    , serviceTier = Nothing
    , stop = Nothing
    , store = Nothing
    , stream = Nothing
    , streamOptions = Nothing
    , temperature = Nothing
    , toolChoice = Nothing
    , tools = Nothing
    , topLogprobs = Nothing
    , topP = Nothing
    , user = Nothing
    , webSearchOptions = Nothing
    , audio = Nothing
    }

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

data OpenAIStreamHandler = OpenAIStreamHandler
  { onToken :: ChatCompletionChunk -> IO ()
  , onComplete :: IO ()
  }

createChatCompletionStream ::
  Text -> ChatCompletionRequest -> OpenAIStreamHandler -> IO (Either String ())
createChatCompletionStream apiKey r OpenAIStreamHandler {..} = do
  request_ <- parseRequest "POST https://api.openai.com/v1/chat/completions"
  let httpReq =
        setRequestHeader "Authorization" ["Bearer " <> encodeUtf8 apiKey] $
          setRequestHeader "Content-Type" ["application/json"] $
            setRequestBodyJSON r $
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
                Just chunk -> onToken chunk
                Nothing -> do
                  -- Handle potential partial JSON by buffering
                  oldBuffer <- readIORef bufferRef
                  let newBuffer = oldBuffer <> content
                  writeIORef bufferRef newBuffer
                  -- Try to parse the combined buffer
                  case decode (LBS.fromStrict newBuffer) of
                    Just chunk -> do
                      onToken chunk
                      writeIORef bufferRef BS.empty -- Clear buffer after successful parse
                    Nothing -> return () -- Keep in buffer for next chunk
        else return () -- Ignore non-data lines
