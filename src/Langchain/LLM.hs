{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Langchain.LLM
    (Params (..), LLM (..), Message (..))
    where

import Data.Text (Text)
import Data.List.NonEmpty (NonEmpty)
import Data.Aeson
import GHC.Generics

data Params = Params {
    tempreture :: Maybe Double
  , maxTokens :: Maybe Integer
  , topP :: Maybe Double
  , n :: Maybe Int
  , stop :: Maybe [Text]
 } deriving (Show, Eq)

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
