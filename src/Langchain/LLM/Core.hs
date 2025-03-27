{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Langchain.LLM.Core
  ( Params (..)
  , StreamHandler (..)
  , LLM (..)
  , Message (..)
  , Role (..)
  , defaultParams
  , defaultMessageData 
  ) where

import Data.Aeson
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import GHC.Generics

data Params = Params
  { temperature :: Maybe Double
  , maxTokens :: Maybe Integer
  , topP :: Maybe Double
  , n :: Maybe Int
  , stop :: Maybe [Text]
  }
  deriving (Show, Eq)

data StreamHandler = StreamHandler
  { onToken :: Text -> IO ()
  , onComplete :: IO ()
  }

data Role = System | User | Assistant | Tool
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data Message = Message
  { role :: Role
  , content :: Text
  , messageData :: MessageData
  }
  deriving (Eq, Show)

-- | A unified type for all various types values that can come in Message e.g 
-- Tool_calls, name etc. use defaultMessageData for normal usage without worrying about 
-- breaking changes.
data MessageData = MessageData
    { name :: Maybe Text
    , toolCalls :: Maybe [Text]
    }
    deriving (Eq, Show)

instance ToJSON MessageData where
    toJSON MessageData{..} = object
        [ "name" .= name
        , "tool_calls" .= toolCalls
        -- Add more fields as they are added
        ]

instance FromJSON MessageData where
    parseJSON = withObject "MessageData" $ \v -> MessageData
        <$> v .:? "name"
        <*> v .:? "tool_calls"

class LLM m where
  invoke :: m -> Text -> Maybe Params -> IO (Either String Text)
  chat :: m -> NonEmpty Message -> Maybe Params -> IO (Either String Text)
  stream :: m -> NonEmpty Message -> StreamHandler -> Maybe Params -> IO (Either String ())


defaultMessageData :: MessageData
defaultMessageData = MessageData {
    name = Nothing
  , toolCalls = Nothing
} 

defaultParams :: Params
defaultParams =
  Params
    { temperature = Nothing
    , maxTokens = Nothing
    , topP = Nothing
    , n = Nothing
    , stop = Nothing
    }
