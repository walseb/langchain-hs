---
sidebar_position: 9
---

# Retriever
`Retriever` typeclass provides an interface to retrieve documents from a collection. It is used in conjunction with the `VectorStore` typeclass to retrieve documents based on their embeddings. The `Retriever` typeclass defines methods for retrieving documents based on their IDs, distances, and other criteria.

It also provides methods for managing the metadata associated with the documents, such as their IDs and distances. The `Retriever` typeclass is designed to be flexible and extensible, allowing developers to implement their own retrievers with custom retrieval algorithms and criteria.

`_get_relevant_documents` method is used to retrieve documents based on their embeddings. It takes a query and returns a list of relevant documents. The `Retriever` typeclass is used in conjunction with the `VectorStore` typeclass to provide a complete solution for managing and retrieving embeddings in Langchain. By implementing the `Retriever` typeclass, developers can create custom retrieval solutions that meet their specific needs and requirements.

## Supported Integrations
At this moment, following integrations available,
- Ollama
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