-- | The Langchain library provides a Haskell interface for interacting with
-- language models, inspired by the Python Langchain library. It defines a
-- typeclass for language models and provides implementations for specific
-- backends like Ollama.
--
-- This module re-exports the core types and the Ollama implementation for
-- convenient access.
module Langchain (
    module Export
 ) where

import Langchain.LLM.Core as Export
import Langchain.LLM.Ollama as Export
import Langchain.Agents.Core as Export
import Langchain.Agents.React as Export
import Langchain.DocumentLoader.Core as Export
import Langchain.DocumentLoader.FileLoader as Export
import Langchain.DocumentLoader.PdfLoader as Export
import Langchain.Embeddings.Ollama as Export
import Langchain.Embeddings.Core as Export
import Langchain.Memory.Core as Export
import Langchain.OutputParser.Core as Export
import Langchain.Retriever.MultiQueryRetriever as Export
import Langchain.Retriever.Core as Export
import Langchain.Tool.Core as Export
import Langchain.VectorStore.Core as Export
import Langchain.Callback as Export
import Langchain.PromptTemplate as Export
