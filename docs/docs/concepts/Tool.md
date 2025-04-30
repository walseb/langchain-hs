---
sidebar_position: 11
---

# Tool 

`Tool` typeclass provides an interface to define a tool that can be used in a chain. runTool is a function that takes an input and produces an output. The `Tool` typeclass defines methods for executing the tool, as well as for managing the metadata associated with the tool, such as its name and description.

The `Tool` typeclass is designed to be flexible and extensible, allowing developers to implement their own tools with custom input and output types. The `Tool` typeclass is used in conjunction with the `Chain` typeclass to provide a complete solution for building and executing chains of tools. By implementing the `Tool` typeclass, developers can create custom tools that meet their specific needs and requirements.

## Supported Integrations

At this moment, following integrations available,

- WikipediaTool
- WebScraperTool

## Example 

```haskell
{-# LANGUAGE OverloadedStrings #-}

module ScrapperTool (runApp) where

import Data.List.NonEmpty (fromList)
import Langchain.LLM.Core
import Langchain.LLM.Ollama
import Langchain.Tool.Core
import Langchain.Tool.WebScraper

runApp :: IO ()
runApp = do
    eRes <- runTool WebScraper "https://tushar-adhatrao.in"
    case eRes of
        Left _ -> pure ()
        Right htmlContent -> do
            let o = Ollama "qwen3:4b" []
            e <-
                chat
                    o
                    ( fromList
                        [ Message
                            System
                            ( "Answer questions based on given below scraped html content: "
                                <> htmlContent
                            )
                            defaultMessageData
                        , Message
                            User
                            "Who is Tushar? /no_think"
                            defaultMessageData
                        ]
                    )
                    Nothing
            print e
```

#### Output

```bash
Right "<think>\n\n</think>\n\nTushar is a friendly neighborhood functional programmer. He is known for his skills in areas such as Haskell, PostgreSQL, Docker, GraphQL, Kubernetes, and GCP. He is currently learning or interested in Rust, Web Assembly, and Large Language Models (LLMs). You can find more about him on his LinkedIn profile, GitHub, and contact him via email."
```

:::warning

Usually, the scraped content can be very large and may not fit into Ollama's chat window. Rather than throwing an error, Ollama seems to ignore the older chat 
messages. 
Hence, it is recommended to break down the scraped content into a list of `Document` and pass it through a `vector store`.

:::
