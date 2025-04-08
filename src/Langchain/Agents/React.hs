{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Langchain.Agents.React
Description : Implementation of ReAct agent combining reasoning and action
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>

Implements the ReAct pattern where the agent alternates between:

1. Reasoning (generating thoughts)
2. Acting (executing tools)

Example agent interaction:

> agent <- createReactAgent llm [wikipediaTool, calculatorTool]
> result <- runAgentExecutor executor "What's the population of Paris?"
> -- Agent might:
> -- 1. Use Wikipedia tool to find current population data
> -- 2. Use calculator tool to verify numbers
> -- 3. Return final answer
-}
module Langchain.Agents.React
  ( ReactAgentOutputParser (..)
  , parseReactOutput
  , ReactAgent (..)
  , createReactAgent
  , formatToolDescriptions
  , formatToolNames
  , getLastUserInput
  ) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Langchain.Agents.Core
import Langchain.LLM.Core
import Langchain.Memory.Core
import Langchain.OutputParser.Core
import Langchain.PromptTemplate
import Langchain.Tool.Core

{- |
Output parser for ReAct agent responses
Handles two primary formats:

1. Final answers containing "Final Answer:"
2. Action requests with "Action:" and "Action Input:"

Example parsing:

> parseReactOutput "Final Answer: 42"
> -- Right (Finish ...)
>
> parseReactOutput "Action: calculator\nAction Input: 5+3"
> -- Right (Continue ...)
-}
newtype ReactAgentOutputParser = ReactAgentOutputParser AgentStep

instance OutputParser ReactAgentOutputParser where
  parse = parseReactOutput

-- | Parses the output from a React agent
parseReactOutput :: Text -> Either String ReactAgentOutputParser
parseReactOutput text
  | T.isInfixOf "Final Answer:" text =
      -- Extract the final answer
      let answer = extractAfter "Final Answer:" text
       in Right $
            ReactAgentOutputParser $
              Finish $
                AgentFinish
                  { returnValues = Map.singleton "output" answer
                  , finishLog = text
                  }
  | T.isInfixOf "Action:" text && T.isInfixOf "Action Input:" text =
      -- Extract action and action input
      let actionName = extractAfter "Action:" $ T.takeWhile (/= '\n') $ T.dropWhile (/= 'A') text
          actionInput_ =
            extractAfter "Action Input:" $ T.takeWhile (/= '\n') $ snd $ T.breakOn "Action Input:" text
       in Right $
            ReactAgentOutputParser $
              Continue $
                AgentAction
                  { actionToolName = T.strip actionName
                  , actionInput = T.strip actionInput_
                  , actionLog = text
                  }
  | otherwise = Left $ "Could not parse agent output: " <> T.unpack text

{- |
Core ReAct agent configuration.
Contains:

- LLM for reasoning
- Available tools
- Prompt template for interaction

Example creation:

> agent <- createReactAgent
>   openAIGPT
>   [ AnyTool wikipediaTool
>   , AnyTool calculatorTool
>   ]
-}
data (LLM llm) => ReactAgent llm = ReactAgent
  { reactLLM :: llm
  , reactTools :: [AnyTool]
  , reactPromptTemplate :: PromptTemplate
  }

-- Helper function to extract text after a marker
extractAfter :: Text -> Text -> Text
extractAfter marker text =
  let afterMarker = snd $ T.breakOn marker text
   in if T.null afterMarker
        then ""
        else T.strip $ T.dropWhile (/= ':') afterMarker

{- |
Creates a ReAct agent with standard prompt structure
The prompt instructs the LLM to:

1. List available tools
2. Follow thought-action-observation pattern
3. Provide final answers

Example prompt excerpt:

> "Use the following format:
> Thought: ...
> Action: [tool_name]
> Action Input: ..."
-}
createReactAgent ::
  (LLM llm) =>
  llm ->
  [AnyTool] ->
  IO (Either String (ReactAgent llm))
createReactAgent llm tools = do
  let reactPrompt =
        PromptTemplate $
          T.unlines
            [ "You are an AI assistant designed to help with tasks."
            , "You have access to the following tools:"
            , "{tools_description}"
            , ""
            , "Use the following format:"
            , ""
            , "Thought: you should always think about what to do"
            , "Action: the action to take, should be one of [{tool_names}]"
            , "Action Input: the input to the action"
            , "Observation: the result of the action"
            , "... (this Thought/Action/Action Input/Observation can repeat N times)"
            , "Thought: I now know the final answer"
            , "Final Answer: the final answer to the original input question"
            ]
  return $
    Right $
      ReactAgent
        { reactLLM = llm
        , reactTools = tools
        , reactPromptTemplate = reactPrompt
        }

instance (LLM llm) => Agent (ReactAgent llm) where
  -- \|
  --  Core reasoning loop implementing ReAct pattern
  --
  --  1. Retrieve chat history
  --  2. Format tool information
  --  3. Construct reasoning prompt
  --  4. Execute LLM call
  --  5. Parse response into action/answer
  --
  --  Uses depth-first planning with backtracking
  --
  planNextAction ReactAgent {..} state = do
    let mem = agentMemory state
    msgResult <- messages mem
    case msgResult of
      Left err -> return $ Left err
      Right msgs -> do
        -- Format the tools descriptions
        let toolDescs = formatToolDescriptions reactTools
            userQuery = getLastUserInput msgs
        -- Build the prompt variables
        let promptVars =
              Map.fromList
                [ ("tools_description", toolDescs)
                , ("tool_names", formatToolNames reactTools)
                ]

        -- Render the prompt
        case renderPrompt reactPromptTemplate promptVars of
          Left err -> return $ Left err
          Right renderedPrompt -> do
            -- Call the LLM
            let m =
                  ( msgs
                      `NE.append` NE.fromList
                        [ (Message System renderedPrompt defaultMessageData)
                        , (Message User userQuery defaultMessageData)
                        ]
                  )
            response <-
              chat
                reactLLM
                m
                Nothing
            case response of
              Left err -> return $ Left err
              Right llmOutput -> do
                -- Parse the output
                case parse llmOutput of
                  Left err -> return $ Left $ "Failed to parse LLM output: " <> err
                  Right (ReactAgentOutputParser step) -> return $ Right step

  agentPrompt ReactAgent {..} = pure reactPromptTemplate
  agentTools ReactAgent {..} = pure reactTools

{- |
Formats tool descriptions for LLM consumption
Creates a list like:

> "Tool: wikipedia
>  Description: Search Wikipedia..."
-}
formatToolDescriptions :: [AnyTool] -> Text
formatToolDescriptions tools = T.intercalate "\n\n" $ map formatTool tools
  where
    formatTool (AnyTool tool _ _) =
      T.concat ["Tool: ", toolName tool, "\nDescription: ", toolDescription tool]

{- |
Creates comma-separated tool names for prompt inclusion
Example output: "wikipedia, calculator, weather"
-}
formatToolNames :: [AnyTool] -> Text
formatToolNames tools = T.intercalate ", " $ map (\(AnyTool tool _ _) -> toolName tool) tools

{- |
Extracts latest user query from chat history
Handles cases where:

- Multiple user messages exist
- No user input found
-}
getLastUserInput :: ChatMessage -> Text
getLastUserInput msgs =
  let userMsgs = filter (\m -> role m == User) $ NE.toList msgs
   in if null userMsgs
        then ""
        else content $ last userMsgs
