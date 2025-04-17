# 🦜️🔗LangChain Haskell

⚡ Building applications with LLMs through composability in Haskell! ⚡

## Introduction

LangChain Haskell is a robust port of the original [LangChain](https://github.com/langchain-ai/langchain) library, bringing its powerful natural language processing capabilities to the Haskell ecosystem. This library enables developers to build applications powered by large language models (LLMs) with ease and flexibility.

## Features

- **LLM Integration**: Seamlessly interact with various language models, including OpenAI's GPT series and others.
- **Prompt Templates**: Create and manage dynamic prompts for different tasks.
- **Memory Management**: Implement conversational memory to maintain context across interactions.
- **Agents and Tools**: Develop agents that can utilize tools to perform complex tasks.
- **Document Loaders**: Load and process documents from various sources for use in your applications.

## Current Supported Providers

  - Ollama
  - More to come...

## Installation

To use LangChain Haskell in your project, add it to your package dependencies. 
If you're using Stack, include it in your `package.yaml`:

```yaml
dependencies:
  - base >= 4.7 && < 5
  - langchain-hs
```
Then, run the build command for your respective build tool to fetch and compile the dependency.

## Quickstart

Here's a simple example demonstrating how to use LangChain Haskell to interact with an LLM:

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Langchain.LLM.Ollama
import Langchain.LLM.Core
import Langchain.PromptTemplate
import Langchain.Callback
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

main :: IO ()
main = do 
  let ollamaLLM = Ollama "llama3.2" [stdOutCallback]
      prompt = PromptTemplate "Translate the following English text to French: {text}"
      input = Map.fromList [("text", "Hello, how are you?")]
      
  case renderPrompt prompt input of
    Left e -> putStrLn $ "Error: " ++ e
    Right renderedPrompt -> do
      eRes <- generate ollamaLLM renderedPrompt Nothing
      case eRes of
        Left err -> putStrLn $ "Error: " ++ err
        Right response -> putStrLn $ "Translation: " ++ (T.unpack response)
```

## Documentation

Documentation will soon be available on hackage.

## Examples

Explore the `examples` directory in the repository for more use cases, including:

- **Conversational Agents**: Building chatbots that maintain context.
- **Document Q&A**: Answering questions based on the content of provided documents.
- **Tool Use**: Creating agents that can use external tools to fetch information or perform calculations.

## Contributing

Contributions are welcome! If you'd like to contribute, please fork the repository and submit a pull request. 
For major changes, please open an issue first to discuss what you'd like to change.

## License

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.

## Acknowledgements

This project is inspired by and builds upon the original [LangChain](https://github.com/langchain-ai/langchain) library and its various ports in other programming languages. 
Special thanks to the developers of those projects for their foundational work.
