{-# LANGUAGE OverloadedStrings #-}

module Langchain.PromptTemplate
  ( PromptTemplate(..)
  , renderPrompt
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as HM

-- | Represents a prompt template with a template string.
-- The template string can contain placeholders of the form {key},
-- where key is a sequence of alphanumeric characters and underscores.
newtype PromptTemplate = PromptTemplate
    { templateString :: Text
    } deriving (Show, Eq)

-- | Render a prompt template with the given variables.
-- Returns either an error message if a variable is missing or the rendered template.
renderPrompt :: PromptTemplate -> HM.Map Text Text -> Either String Text
renderPrompt (PromptTemplate template) vars = interpolate vars template

-- | Interpolate variables into a template string.
-- Placeholders are of the form {key}, where key is a sequence of alphanumeric characters and underscores.
interpolate :: HM.Map Text Text -> Text -> Either String Text
interpolate vars template = go template
  where
    go :: Text -> Either String Text
    go t =
      case T.breakOn "{" t of
        (before, after) | T.null after -> Right before
        (before, after') ->
          case T.breakOn "}" (T.drop 1 after') of
            (_, after'') | T.null after'' -> Left "Unclosed brace"
            (key, after''') ->
              let key' = T.strip key
              in case HM.lookup key' vars of
                Just val -> do
                  rest <- go (T.drop 1 after''')
                  return $ before <> val <> rest
                Nothing -> Left $ "Missing variable: " <> T.unpack key'