module Langchain.LLM
    (Params (..), LLM (..))
    where

import Data.Text (Text)

data Params = Params {
    tempreture :: Maybe Double
  , maxTokens :: Maybe Integer
  , topP :: Maybe Double
  , n :: Maybe Int
  , stop :: Maybe [Text]
 } deriving (Show, Eq)

class LLM m where
   call :: m -> Text -> Maybe Params -> IO Text
