---
sidebar_position: 4
---

# Memory

In langchain-hs, Memory module provides a `BaseMemory` typeclass. The goal is provide types with 
BaseMemory instance, that can easily access and manipulate `Chat History`.

So far, below BaseMemory instances are defined:

- WindowBufferMemory
- TokenBufferMemory
- More to come...

## WindowBufferMemory

`WindowBufferMemory` type provides a mechanism to store chat history with a `max window size`. Once it the conversation crosses that limit, the Type will omit the older conversation and keep the window size intact.
It is basic but helpful mechanism to keep the token size.

[Here](https://python.langchain.com/v0.1/docs/modules/memory/types/buffer_window/) is the python documentation which WindowBufferMemory is inspired from.

## TokenBufferMemory

`TokenBufferMemory` type provides a mechanism to store chat history with a `max token size`. Once it the conversation crosses that limit, the Type will omit the older conversation and keep the token size intact. 1 token is equal to 4 characters as per (link)[https://help.openai.com/en/articles/4936856-what-are-tokens-and-how-to-count-them].

### Example

```haskell
{-# LANGUAGE OverloadedStrings #-}

module LangchainLib (runApp) where

import Langchain.LLM.Core
import Langchain.LLM.Ollama
import qualified Data.List.NonEmpty as NE
import Langchain.Memory.TokenBufferMemory
import Langchain.Memory.Core

runApp :: IO ()
runApp = do
    let o = Ollama "qwen3:4b" []
    let tokenBuffMsgs = TokenBufferMemory {
        maxTokens = 30
      , tokenBufferMessages = NE.fromList [
          Message User "Hey! how are you? /no_think" defaultMessageData
      ]
    }
    eMsgs <- messages tokenBuffMsgs
    case eMsgs of
      Left _ -> pure () 
      Right msgs -> do
        e <-
            chat
                o
                msgs
                Nothing
        print e
```