
{-# LANGUAGE OverloadedStrings #-}

module Embeddings where

import qualified Data.Text as T

-- A dummy type representing an embedding vector.
type Embedding = [Double]

-- A simple dummy embedding function.
embedText :: T.Text -> IO Embedding
embedText txt = do
  let wordsList = T.words txt
      vec = map (fromIntegral . (`mod` 10) . T.length) wordsList
  return vec

main :: IO ()
main = do
  putStrLn "=== Embeddings Example ==="
  let textToEmbed = "Langchain-HS integrates LLMs using Haskell and Ollama."
  embedding <- embedText (T.pack textToEmbed)
  putStrLn "Embedding vector:"
  print embedding
