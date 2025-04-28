---
sidebar_position: 3
---

# Embeddings models

`Embeddings` typeclass provides interface over various Embeddings models.

As of now, below models are provided out of the box:

- OllamaEmbeddings
- More to come...

# Custom embedding model

It is also possible to create your own type and implement Embeddings typeclass.

for e.g

```haskell
data OpenAIEmbedding = OpenAIEmbedding {
  apiKey :: Text,
  apiUrl :: Text,
  model :: Text
}

instance Embeddings Deepseek where
  embedDocuments OpenAIEmbedding docs = do
    -- Your implementation here
    return $ Right []
  embedQuery OpenAIEmbedding{..} query = do
    -- Your implementation here
    return $ Right []
```
