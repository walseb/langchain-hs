
{-# LANGUAGE OverloadedStrings #-}

module OllamaLLM where

import Langchain.LLM.Ollama (Ollama(..))
import Langchain.LLM.Core
  ( LLM(..)
  , Message(..)
  , Role(..)
  , defaultMessageData
  , Params(..)
  , defaultParams
  )
import qualified Data.Text as T
import Data.List.NonEmpty (fromList)

main :: IO ()
main = do
  putStrLn "=== Ollama LLM Example ==="
  let ollamaLLM = Ollama "llama3.2" []  -- Using real Ollama integration with llama3.2
  -- Example: generate a text completion.
  genResult <- generate ollamaLLM "Explain Haskell in simple terms." Nothing
  case genResult of
    Left err -> putStrLn $ "Generate error: " ++ err
    Right text -> putStrLn $ "Generated Text:\n" ++ T.unpack text

  -- Example: chat interaction.
  let chatHistory = fromList
        [ Message System "You are an AI assistant." defaultMessageData
        , Message User "What is functional programming?" defaultMessageData
        ]
  chatResult <- chat ollamaLLM chatHistory Nothing
  case chatResult of
    Left err -> putStrLn $ "Chat error: " ++ err
    Right response -> putStrLn $ "Chat Response:\n" ++ T.unpack response
