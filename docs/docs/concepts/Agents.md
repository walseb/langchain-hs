---
sidebar_position: 12
---

# Agents 

The core idea of agents is to use a language model to choose a sequence of actions to take. In chains, a sequence of actions is hardcoded (in code). In agents, a language model is used as a reasoning engine to determine which actions to take and in which order.

## Example 

Here I have used ReAct agent type find latest information that is not know to LLM using WikipediaTool.

```haskell
{-# LANGUAGE OverloadedStrings #-}

module ReactAgent (runApp) where

import Langchain.Agents.Core
import Langchain.Agents.React
import Langchain.LLM.Ollama
import Langchain.Tool.WikipediaTool

runApp :: IO ()
runApp = do
    let ollamaLLM =
            Ollama
                { modelName = "qwen3:4b"
                , callbacks = []
                }
    let anyWikiTool =
            customAnyTool defaultWikipediaTool id id
    eRes <-
        runReactAgent
            ollamaLLM
            Nothing
            [anyWikiTool]
            "When is james gunn's superman movie releasing /no_think"
    print eRes
```

#### Output 

```
Right (AgentFinish {returnValues = fromList [("output","James Gunn's Superman movie is scheduled to be released theatrically in the United States on July 11, 2025.")], finishLog = "<think>\n\n</think>\n\nThought: I now know the final answer  \nFinal Answer: James Gunn's Superman movie is scheduled to be released theatrically in the United States on July 11, 2025."})
```
