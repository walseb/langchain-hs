---
sidebar_position: 4
---

# Memory

In langchain-hs, Memory module provides a `BaseMemory` typeclass. The goal is provide types with 
BaseMemory instance, that can easily access and manipulate `Chat History`.

So far, below BaseMemory instances are defined:

- WindowBufferMemory
- More to come...

## WindowBufferMemory

`WindowBufferMemory` type provides a mechanism to store chat history with a `max window size`. Once it the conversation crosses that limit, the Type will omit the older conversation and keep the window size intact.
It is basic but helpful mechanism to keep the token size.

[Here](https://python.langchain.com/v0.1/docs/modules/memory/types/buffer_window/) is the python documentation which WindowBufferMemory is inspired from.
