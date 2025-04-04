{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Langchain.PromptTemplate
  ( PromptTemplate (..)
  , renderPrompt
  , FewShotPromptTemplate (..)
  , renderFewShotPrompt
  ) where

import qualified Data.Map.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T
import Langchain.Runnable.Core (Runnable (..))

-- TODO: Add Mechanism for custom example selector

{- | Represents a prompt template with a template string.
The template string can contain placeholders of the form {key},
where key is a sequence of alphanumeric characters and underscores.
-}
newtype PromptTemplate = PromptTemplate
  { templateString :: Text
  }
  deriving (Show, Eq)

{- | Render a prompt template with the given variables.
Returns either an error message if a variable is missing or the rendered template.
-}
renderPrompt :: PromptTemplate -> HM.Map Text Text -> Either String Text
renderPrompt (PromptTemplate template) vars = interpolate vars template

-- | Represents a few-shot prompt template with examples.
data FewShotPromptTemplate = FewShotPromptTemplate
  { fsPrefix :: Text
  -- ^ Text before the examples
  , fsExamples :: [HM.Map Text Text]
  -- ^ List of example variable maps
  , fsExampleTemplate :: Text
  -- ^ Template for formatting each example
  , fsExampleSeparator :: Text
  -- ^ Separator between formatted examples
  , fsSuffix :: Text
  -- ^ Text after the examples, with placeholders
  , fsInputVariables :: [Text]
  -- ^ Expected variables in the suffix
  }
  deriving (Show, Eq)

{- | Render a few-shot prompt template with the given input variables.
Returns either an error message if interpolation fails or the fully rendered prompt.
-}
renderFewShotPrompt :: FewShotPromptTemplate -> HM.Map Text Text -> Either String Text
renderFewShotPrompt FewShotPromptTemplate {..} inputVars = do
  -- Format each example using the example template
  formattedExamples <- mapM (\ex -> interpolate ex fsExampleTemplate) fsExamples
  -- Join the formatted examples with the separator
  let examplesText = T.intercalate fsExampleSeparator formattedExamples
  -- Format the suffix with the input variables
  formattedSuffix <- interpolate inputVars fsSuffix
  -- Combine prefix, examples, and suffix
  return $ fsPrefix <> examplesText <> formattedSuffix

{- | Interpolate variables into a template string.
Placeholders are of the form {key}, where key is a sequence of alphanumeric characters and underscores.
-}
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

instance Runnable PromptTemplate where
  type RunnableInput PromptTemplate = HM.Map Text Text
  type RunnableOutput PromptTemplate = Text

  invoke template variables = pure $ renderPrompt template variables