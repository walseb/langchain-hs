{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Langchain.Tool.Core
  ( Tool(..)
  ) where

import Data.Text (Text)

-- | Typeclass defining the interface for tools.
-- Each tool must specify its input and output types and implement methods for
-- retrieving its name, description, and executing the tool.
class Tool a where
  -- | The type of input the tool expects.
  type Input a
  -- | The type of output the tool produces.
  type Output a
  -- | Retrieves the name of the tool.
  toolName :: a -> Text
  -- | Retrieves a description of the tool.
  toolDescription :: a -> Text
  -- | Executes the tool with the given input and returns the output in IO.
  runTool :: a -> Input a -> IO (Output a)

