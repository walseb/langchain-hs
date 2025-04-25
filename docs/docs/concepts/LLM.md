---
sidebar_position: 1
---

# LLM

The `LLM` typeclass is the cornerstone of langchain-hs. It provides a unified interface for:

 - Text generation
 - Chat conversations
 - Streaming responses
 - Custom parameter handling

Every type that has LLM instance, can call generate, chat and stream.

## Supported Integrations

At this moment, following integrations available,

 - Ollama
 - OpenAI
 - HuggingFace
 - More to come...

## Custom

It is also possible to create your own type and implement LLM class.

```haskell
class LLM a where
  generate :: a -> Text -> Maybe Params -> IO (Either String Text)
  chat :: a -> NonEmpty Message -> Maybe Params -> IO (Either String Text)
  stream :: a -> NonEmpty Message -> StreamHandler -> Maybe Params -> IO (Either String ())
```

for e.g

```haskell

data Deepseek = Deepseek {
  apiKey :: Text,
  apiUrl :: Text
  model :: Text
}

instance LLM Deepseek where
  generate (Deepseek apiKey apiUrl model) prompt params = do
    -- Your implementation here
    return $ Right "Generated text"
  chat (Deepseek apiKey apiUrl model) messages params = do
    -- Your implementation here
    return $ Right "Chat response"
  stream (Deepseek apiKey apiUrl model) messages handler params = do
    -- Your implementation here
    return $ Right ()

```
