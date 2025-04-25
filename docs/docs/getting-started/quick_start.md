---
sidebar_position: 1
---
import Tabs from '@theme/Tabs';
import TabItem from '@theme/TabItem';

# Quickstart: Langchain-hs

### Requirements

For using `Ollama LLM`, Make sure you have [Ollama](https://ollama.com/) installed.

### Add langchain-hs package in your dependancies 

```yaml title="stack.yaml"
dependancies:
  base,
  langchain-hs
```

### Example of generating response from a single prompt.

<Tabs
  defaultValue="ollama"
  values={[
    {label: 'Ollama', value: 'ollama'},
    {label: 'OpenAI', value: 'openai'},
    {label: 'Huggingface', value: 'huggingface'}
  ]}>
  <TabItem value="ollama">
    ```haskell
    {-# LANGUAGE OverloadedStrings #-}

    module LangchainLib (runApp) where

    import Langchain.LLM.Ollama (Ollama(..))
    import Langchain.LLM.Core
    import qualified Data.Text as T

    runApp :: IO ()
    runApp = do
    let ollamaLLM = Ollama "llama3.2" []  
    genResult <- generate ollamaLLM "Explain Haskell in simple terms." Nothing
    case genResult of
        Left err -> putStrLn $ "Generate error: " ++ err
        Right text -> putStrLn $ "Generated Text:\n" ++ T.unpack text
    ```
  </TabItem>
  <TabItem value="openai">
  ```haskell
  {-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text)
import qualified Langchain.LLM.Core as LLM
import Langchain.LLM.OpenAI (OpenAI(..))

main :: IO ()
main = do
  let openAI = OpenAI
        { apiKey = "your-api-key"
        , openAIModelName = "gpt-4.1-nano"
        , callbacks = []
        }
  result <- LLM.generate openAI "Tell me a joke" Nothing
  case result of
    Left err -> putStrLn $ "Error: " ++ err
    Right response -> putStrLn response
    ```
  </TabItem>
  <TabItem value="huggingface">
```haskell
{-# LANGUAGE OverloadedStrings #-}

module LangchainLib (runApp) where

import qualified Data.Text as T
import Langchain.LLM.Core
import Langchain.LLM.Huggingface 

runApp :: IO ()
runApp = do
    let huggingface =
            Huggingface
                { provider = Cerebras
                , apiKey = <your-api-key>
                , modelName = "llama-3.3-70b"
                , callbacks = []
                }
    eRes <- generate huggingface "Explain me Monads in Haskell" Nothing
    case eRes of
      Left err -> putStrLn $ "Chat error: " ++ err
      Right response -> putStrLn $ "Chat Response:\n" ++ T.unpack response
```
  </TabItem>
</Tabs>



In above code, I have intialized the `Ollama` with model name `llama3.2`. Then I can simply call generate function with ollamaLLM and the prompt.
It will either return a Error or the response Text.

:::warning

For Ollama, Make sure the model that you want to use is installed on your local machine. Else it will throw error.

:::

### Example of generating response from a chat history

<Tabs
  defaultValue="ollama"
  values={[
    {label: 'Ollama', value: 'ollama'},
    {label: 'OpenAI', value: 'openai'},
    {label: 'Huggingface', value: 'huggingface'}
  ]}>
  <TabItem value="ollama">
```haskell
{-# LANGUAGE OverloadedStrings #-}

module LangchainLib (runApp) where

import Langchain.LLM.Ollama (Ollama(..))
import Langchain.LLM.Core
import qualified Data.Text as T
import Data.List.NonEmpty (fromList)

runApp :: IO ()
runApp = do
  let ollamaLLM = Ollama "llama3.2" []  
  let chatHistory = fromList
        [ Message System "Explain everthing with a texas accent." defaultMessageData
        , Message User "What is functional programming?" defaultMessageData
        ]
  chatResult <- chat ollamaLLM chatHistory Nothing
  case chatResult of
    Left err -> putStrLn $ "Chat error: " ++ err
    Right response -> putStrLn $ "Chat Response:\n" ++ T.unpack response
```
    </TabItem>
    <TabItem value="openai">
  ```haskell
  {-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Langchain.LLM.Core as LLM
import qualified Data.Text as T
import Data.List.NonEmpty (fromList)

main :: IO ()
main = do
  let openAI = OpenAI
        { apiKey = "your-api-key"
        , openAIModelName = "gpt-4.1-nano"
        , callbacks = []
        }
  let chatHistory = fromList
        [ Message System "You are an AI assistant." defaultMessageData
        , Message User "What is functional programming?" defaultMessageData
        ]
  chatResult <- LLM.chat openAI chatHistory Nothing
  case chatResult of
    Left err -> putStrLn $ "Chat error: " ++ err
    Right response -> putStrLn $ "Chat Response:\n" ++ T.unpack response
    ```
  </TabItem>
  <TabItem value="huggingface">
```haskell
{-# LANGUAGE OverloadedStrings #-}

module LangchainLib (runApp) where

import qualified Data.Text as T
import Langchain.LLM.Core 
import Langchain.LLM.Huggingface
import Data.List.NonEmpty (fromList)

runApp :: IO ()
runApp = do
    let huggingface =
            Huggingface
                { provider = Cerebras
                , apiKey = <your-api-key>
                , modelName = "llama-3.3-70b"
                , callbacks = []
                }
    let chatHistory = fromList [ 
            Message System "You are an AI assistant." defaultMessageData
          , Message User "What is functional programming?" defaultMessageData]
    eRes <- chat huggingface chatHistory Nothing
    case eRes of
      Left err -> putStrLn $ "Chat error: " ++ err
      Right response -> putStrLn $ "Chat Response:\n" ++ T.unpack response
```
  </TabItem>
    </Tabs>


There are several helper functions provided to manipulate `chatHistory`

### Example of streaming response

<Tabs
  defaultValue="ollama"
  values={[
    {label: 'Ollama', value: 'ollama'},
    {label: 'OpenAI', value: 'openai'},
    {label: 'Huggingface', value: 'huggingface'}
  ]}>
  <TabItem value="ollama">
```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Langchain.LLM.Ollama (Ollama(..))
import Langchain.LLM.Core
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.List.NonEmpty (fromList)

main :: IO ()
main = do
  let ollamaLLM = Ollama "llama3.2" []  
  let chatHistory = fromList
        [ Message System "You are an AI assistant." defaultMessageData
        , Message User "What is functional programming?" defaultMessageData
        ]
  let handler = StreamHandler T.putStr (putStrLn "Response complete")
  eRes <- stream ollamaLLM chatHistory handler Nothing 
  case eRes of
    Left err -> putStrLn $ "Chat error: " ++ err
    Right _ -> pure ()
```
    </TabItem>
    <TabItem value="openai">
```haskell
{-# LANGUAGE OverloadedStrings #-}

module LangchainLib (runApp) where

import Data.List.NonEmpty (fromList)
import qualified Data.Text.IO as T
import Langchain.LLM.Core as LLM
import Langchain.LLM.OpenAI

runApp :: IO ()
runApp = do
    let openAI =
            OpenAI
                { apiKey = "your-api-key"
                , openAIModelName = "gpt-4.1-nano"
                , callbacks = []
                }
    let chatHistory =
            fromList
                [ Message System "You are an AI assistant." defaultMessageData
                , Message User "What is functional programming?" defaultMessageData
                ]
    let streamHandler = StreamHandler {
        onToken = T.putStr,
        onComplete = pure ()
    }
    chatResult <- LLM.stream openAI chatHistory streamHandler Nothing
    case chatResult of
        Left err -> putStrLn $ "Chat error: " ++ err
        Right _ -> pure ()
        ```
    </TabItem>
    <TabItem value="huggingface">
```haskell
{-# LANGUAGE OverloadedStrings #-}

module LangchainLib (runApp) where

import Data.List.NonEmpty (fromList)
import qualified Data.Text.IO as T
import Langchain.LLM.Core as LLM
import Langchain.LLM.Huggingface

runApp :: IO ()
runApp = do
    let huggingface =
            Huggingface
                { provider = Cerebras
                , apiKey = "your-api-key"
                , modelName = "llama-3.3-70b"
                , callbacks = []
                }
    let chatHistory = fromList [Message System "You are an AI assistant." defaultMessageData, Message User "What is functional programming?" defaultMessageData]
    let streamHandler =
            StreamHandler
                { onToken = T.putStr
                , onComplete = pure ()
                }
    eRes <- stream huggingface chatHistory streamHandler Nothing
    case eRes of
        Left err -> putStrLn $ "Chat error: " ++ err
        Right _ -> pure ()
        ```
    </TabItem>
    </Tabs>

For streaming you are supposed to send a callback function. The callback function will be called for each token generated.
