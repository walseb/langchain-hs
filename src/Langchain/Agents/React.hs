{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Langchain.Agents.React
Description : Implementation of ReAct agent
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com

This module provides implementation of ReAct agent.
-}
module Langchain.Agents.React
  ( ReactAgentOutputParser
  , parseReactOutput
  , ReactAgent (..)
  , createReactAgent
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

-- | Creates a React agent (Reasoning and Acting) with the given LLM, tools, and memory
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

-- | Format tool descriptions as a string
formatToolDescriptions :: [AnyTool] -> Text
formatToolDescriptions tools = T.intercalate "\n\n" $ map formatTool tools
  where
    formatTool (AnyTool tool _ _) =
      T.concat ["Tool: ", toolName tool, "\nDescription: ", toolDescription tool]

-- | Format tool names as a comma-separated string
formatToolNames :: [AnyTool] -> Text
formatToolNames tools = T.intercalate ", " $ map (\(AnyTool tool _ _) -> toolName tool) tools

-- | Get the last user input from the chat history
getLastUserInput :: ChatMessage -> Text
getLastUserInput msgs =
  let userMsgs = filter (\m -> role m == User) $ NE.toList msgs
   in if null userMsgs
        then ""
        else content $ last userMsgs
