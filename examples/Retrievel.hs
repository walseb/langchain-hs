
{-# LANGUAGE OverloadedStrings #-}

module Retrievel where

import Langchain.DocumentLoader.Core (Document(..))
import Langchain.Retriever.Core (Retriever(..))
import Langchain.Retriever.MultiQueryRetriever
import Langchain.LLM.Ollama (Ollama(..))
import Data.Text (Text)
import qualified Data.Text as T

-- For demonstration, we create a simple base retriever that returns a fixed document.
data DummyRetriever = DummyRetriever

instance Retriever DummyRetriever where
  _get_relevant_documents _ query =
    return $ Right [Document ("Dummy retrieval result for query: " <> query) mempty]

main :: IO ()
main = do
  putStrLn "=== Retrieval Example using MultiQueryRetriever with Ollama (llama3.2) ==="
  let baseRetriever = DummyRetriever
      -- Instantiate Ollama with the "llama3.2" model.
      ollamaLLM = Ollama "llama3.2" []
      mqRetriever = newMultiQueryRetriever baseRetriever ollamaLLM
  
  result <- _get_relevant_documents mqRetriever "What is Langchain-HS?"
  case result of
    Left err -> putStrLn $ "Retrieval error: " ++ err
    Right docs -> do
      putStrLn "Retrieved documents:"
      mapM_ (putStrLn . T.unpack . pageContent) docs
