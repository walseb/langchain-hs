---
sidebar_position: 6
---

# Callbacks

Callbacks are a powerful feature in Langchain that allow you to monitor and control the execution of your chains, agents, and tools. They provide a way to receive updates about the progress of your operations, log information, and even modify the behavior of your chains on-the-fly. 

Callbacks can be used for various purposes, such as logging, debugging, and performance monitoring. They can also be used to implement custom behavior in your chains and agents, such as stopping execution based on certain conditions or modifying the input to a tool.

stdOutCallback is a simple callback that prints the output of each step to the standard output. This can be useful for debugging and monitoring the progress of your chains.

```haskell
let callbacks = [stdOutCallback]
result <- generate (Ollama "llama3.2:latest" callbacks) "What is 2+2?" Nothing
print result
```