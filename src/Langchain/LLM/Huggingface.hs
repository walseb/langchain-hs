{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module:      Langchain.LLM.Huggingface
Copyright:   (c) 2025 Tushar Adhatrao
License:     MIT
Maintainer:  Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability:   experimental

Huggingface inference implementation Langchain's LLM Interface
-}
module Langchain.LLM.Huggingface
  ( Huggingface (..)
  , Provider (..)
  ) where

import Data.Text (Text, unpack)
import Langchain.Callback
import Langchain.LLM.Core
import Langchain.LLM.Internal.Huggingface

{- |
    Providers integrated with Huggingface Inference
    https://huggingface.co/docs/inference-providers/index#partners
-}
data Provider
  = Cerebras
  | Cohere
  | FalAI
  | Fireworks
  | HFInference
  | Hyperbolic
  | Nebius
  | Novita
  | Replicate
  | SambaNova
  | Together
  deriving (Show, Eq)

data Huggingface = Huggingface
  { provider :: Provider
  , apiKey :: Text
  , modelName :: Text
  , callbacks :: [Callback]
  }

instance Show Huggingface where
  show Huggingface {..} =
    "Huggingface { provider = "
      <> show provider
      <> ", modelName = "
      <> unpack modelName
      <> " }"

data HuggingfaceParams = HuggingfaceParams {
    frequencyPenalty :: Maybe Double 
  , maxTokens :: Maybe Integer
  , presencePenalty :: Maybe Double
  , stop :: Maybe [String]
  , toolPrompt :: Maybe String
  , topP :: Maybe Double
 } deriving (Eq, Show)

instance LLM Huggingface where
  type LLMParams Huggingface = Text

