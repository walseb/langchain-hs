module Langchain.Ollama (
    Ollama (..)
 ) where

import Data.Text (Text)
import Langchain.LLM
import qualified Data.Ollama.Generate as Ollama

type Model = Text
newtype Ollama = Ollama Model

instance LLM Ollama where
    call (Ollama model) prompt _ = do
        eRes <-
            Ollama.generate
                Ollama.defaultGenerateOps
                    { Ollama.modelName = model
                    , Ollama.prompt = prompt
                    }
        case eRes of
            Left err -> error err
            Right r -> return (Ollama.response_ r)
