{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Langchain.LLM (
    Params (..),
    LLM (..),
    Message (..),
    Role (..),
    defaultParams,
)
where

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
    , stream :: Maybe (Text -> IO (), IO ())
    }

data Role
    = System
    | User
    | Assistant
    | Tool
    deriving (Eq)

instance FromJSON Role where
    parseJSON (String "system") = return System
    parseJSON (String "user") = return User
    parseJSON (String "assistant") = return Assistant
    parseJSON (String "tool") = return Tool
    parseJSON x = error $ "Cannot parse json value for role " <> show x

instance ToJSON Role where
    toJSON System = String "system"
    toJSON User = String "user"
    toJSON Assistant = String "assistant"
    toJSON Tool = String "tool"

instance Show Role where
    show System = "system"
    show User = "user"
    show Assistant = "assistant"
    show Tool = "tool"

data Message = Message
    { role :: Role
    , content :: Text
    , name :: Maybe Text
    }
    deriving (Eq, Show, Generic, ToJSON, FromJSON)

class LLM m where
    call :: m -> Text -> Maybe Params -> IO Text
    chat :: m -> NonEmpty Message -> Maybe Params -> IO Text

-- Not every paramenter supports each LLM.
-- This type is a unified parameter set of args that will be sent to
-- corrosponding argument.
defaultParams :: Params
defaultParams =
    Params
        { temperature = Nothing
        , maxTokens = Nothing
        , topP = Nothing
        , n = Nothing
        , stop = Nothing
        , stream = Nothing
        }
