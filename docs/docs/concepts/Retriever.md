---
sidebar_position: 9
---

# Retriever

`Retriever` typeclass provides an even more generic interface to retrieve documents than vectorStore.

## Supported Integrations

At this moment, following integrations available,
- VectorStoreRetriever
- More to come...

## Custom Retriever

It is also possible to create your own type and implement `Retriever` typeclass.

```haskell
data CustomRetriever = CustomRetriever {
  apiKey :: Text,
    apiUrl :: Text,
    model :: Text
}
instance Retriever CustomRetriever where
  _get_relevant_documents (CustomRetriever apiKey apiUrl model) query = do
    -- Your implementation here
    return $ Right []
```

## Example

```haskell 
{-# LANGUAGE OverloadedStrings #-}

module RetrieverExample (runApp) where

import Langchain.DocumentLoader.Core
import Langchain.DocumentLoader.PdfLoader
import Langchain.Embeddings.Ollama
import Langchain.Retriever.Core (Retriever (_get_relevant_documents), VectorStoreRetriever (VectorStoreRetriever))
import Langchain.VectorStore.InMemory (fromDocuments)

runApp :: IO ()
runApp = do
    let pdf = PdfLoader "/home/user/Documents/TS/langchain/SOP.pdf"
    eDocs <- load pdf
    case eDocs of
        Left err -> putStrLn $ "Error while reading docs: " <> err
        Right docs -> do
            let ollamaEmb =
                    OllamaEmbeddings
                        { model = "nomic-embed-text:latest"
                        , defaultTruncate = Just True
                        , defaultKeepAlive = Just "5m"
                        }
            eInMemVS <- fromDocuments ollamaEmb docs
            case eInMemVS of
                Left err -> putStrLn $ "Error while initiating Vector Store: " <> err
                Right inMemVS -> do
                    let vsRetriever = VectorStoreRetriever inMemVS
                    eRes <-
                        _get_relevant_documents
                            vsRetriever
                            "I am having with accessing the vendor portal. I have been unable to log in to my account."
                    print eRes
```
