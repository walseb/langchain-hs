{-# LANGUAGE OverloadedStrings #-}

import Langchain.VectorStore.InMemory
import Langchain.VectorStore.Core
import Langchain.Embeddings.Core
import Langchain.DocumentLoader.Core
import Langchain.Embeddings.Ollama
import Data.Map.Strict (empty)

testInMemory :: IO ()
testInMemory = do 
    let doc1 = Document { pageContent = "Hello world", metadata = empty }
        doc2 = Document { pageContent = "Goodbye world", metadata = empty }
        doc3 = Document { pageContent = "Hello again", metadata = empty }
        docs = [doc1, doc2, doc3]
    
    -- Initialize the embedding model
    let embeddingModel = OllamaEmbeddings "nomic-embed-text:latest" Nothing Nothing
    -- Create an empty in-memory vector store
    let vs = emptyInMemoryVectorStore embeddingModel
    -- Add documents to the vector store
    eVs' <- addDocuments vs docs
    case eVs' of
        Left err -> putStrLn $ "Error adding documents: " ++ err
        Right vs' -> do
            -- Successfully added documents, now perform a similarity search
            putStrLn "Documents added successfully."
            -- Perform a similarity search
            result <- similaritySearch vs' "Hello" 2
            case result of
                Left err -> putStrLn $ "Error during similarity search: " ++ err
                Right docs -> print docs