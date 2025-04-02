-- | The Langchain library provides a Haskell interface for interacting with
-- language models, inspired by the Python Langchain library. It defines a
-- typeclass for language models and provides implementations for specific
-- backends like Ollama.
--
-- This module re-exports the core types and the Ollama implementation for
-- convenient access.
module Langchain (module Export) where

import Langchain.LLM.Core as Export
import Langchain.LLM.Ollama as Export
