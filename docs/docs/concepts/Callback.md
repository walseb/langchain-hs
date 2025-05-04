---
sidebar_position: 6
---

# Callbacks

Callbacks are a powerful feature in Langchain that allow you to monitor the execution of your LLMs. They provide a way to receive updates about the progress of your operations, log information on-the-fly. .

`stdOutCallback` is a simple callback that prints the output of each step to the standard output. This can be useful for debugging and monitoring the progress of your LLMs.

```haskell
let callbacks = [stdOutCallback]
result <- generate (Ollama "llama3.2:latest" callbacks) "What is 2+2?" Nothing
print result
```