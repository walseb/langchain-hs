
{-# LANGUAGE OverloadedStrings #-}

module VectorStore where

import Langchain.DocumentLoader.Core (Document(..))
import Langchain.VectorStore.Core (VectorStore(..))
import qualified Data.Text as T
import Control.Monad (forM_)

-- A simple in-memory vector store implementation.
data InMemoryVectorStore = InMemoryVectorStore [Document]

instance VectorStore InMemoryVectorStore where
  -- The similaritySearch function filters documents whose content contains the query substring.
  similaritySearch (InMemoryVectorStore docs) query k = do
    let filtered = filter (\doc -> T.toLower query `T.isInfixOf` T.toLower (pageContent doc)) docs
    return $ Right (take k filtered)

-- A helper to add a Document to the vector store.
addDocument :: InMemoryVectorStore -> Document -> InMemoryVectorStore
addDocument (InMemoryVectorStore docs) doc = InMemoryVectorStore (doc : docs)

main :: IO ()
main = do
  putStrLn "=== VectorStore Example ==="
  let store0 = InMemoryVectorStore []
      doc1 = Document "Haskell is a functional programming language." mempty
      doc2 = Document "Ollama offers LLM services using models like gemma3." mempty
      store  = addDocument (addDocument store0 doc1) doc2
  searchResult <- similaritySearch store "Haskell" 5
  case searchResult of
    Left err -> putStrLn $ "Error: " ++ err
    Right docs -> do
      putStrLn "Search results:"
      forM_ docs (putStrLn . T.unpack . pageContent)
