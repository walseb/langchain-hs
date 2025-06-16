{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Langchain.Agents.React
Description : Implementation of ReAct logic
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>

ReAct forces LLM to reflect on your question and injects responses as if LLM figured them out by itself. 
This allows you to connect any datasource or tool tou your LLM.
-}
module Langchain.Agents.React
  ( defaultReactPromptTemplate
  , runReactAgent
  , ReactAgent (..)
  ) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Langchain.Agents.Core
import Langchain.LLM.Core
import Langchain.Memory.Core
import Langchain.OutputParser.Core
import Langchain.PromptTemplate
import Langchain.Tool.Core

-- | Default system prompt for React Agent
defaultReactPromptTemplate :: PromptTemplate
defaultReactPromptTemplate =
  PromptTemplate $
    T.unlines
      [ "You are an AI assistant designed to help with tasks."
      , "You have access to the following tools:"
      , "{tools}"
      , ""
      , "If you don't know the answer, you can use tool to get the information."
      , "Response in either one or the other format:"
      , "1. If you don't the answer and want to make a tool call:"
      , "Thought: you should always think about what to do"
      , "Action: the action to take, should be one of [{tool_names}]"
      , "Action Input: the input to the action"
      , "... (this Thought/Action/Action Input can repeat N times)"
      , ""
      , "2. If you found out the answer:"
      , "Thought: I now know the final answer"
      , "Final Answer: the final answer to the original input question"
      ]

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
      let actionName =
            extractAfter "Action:" $
              T.takeWhile (/= '\n') $
                T.dropWhile (/= 'A') text
          actionInput_ =
            extractAfter "Action Input:" $
              T.takeWhile (/= '\n') $
                snd $
                  T.breakOn "Action Input:" text
       in Right $
            ReactAgentOutputParser $
              Continue $
                AgentAction
                  { actionToolName = T.strip actionName
                  , actionInput = T.strip actionInput_
                  , actionLog = text
                  }
  | otherwise = Left $ "Could not parse agent output: " <> T.unpack text

-- Helper function to extract text after a marker
extractAfter :: Text -> Text -> Text
extractAfter marker text =
  let afterMarker = snd $ T.breakOn marker text
   in if T.null afterMarker
        then ""
        else T.strip $ T.drop 2 $ T.dropWhile (/= ':') afterMarker

-- | ReactAgent Type
data (LLM llm) => ReactAgent llm = ReactAgent
  { reactLLM :: llm
  , reactLLMParams :: Maybe (LLMParams llm)
  , reactToolList :: [AnyTool]
  }

-- | Run React Agent
runReactAgent ::
  LLM llm =>
  llm ->
  Maybe (LLMParams llm) ->
  [AnyTool] ->
  Text ->
  IO (Either String AgentFinish)
runReactAgent l mbParams tools userQuery = do
  let promptVars =
        Map.fromList
          [ ("tools", formatTools tools)
          , ("tool_names", formatToolNames tools)
          ]
  case renderPrompt defaultReactPromptTemplate promptVars of
    Left err -> pure $ Left err
    Right r -> do
      let reactAgent =
            ReactAgent
              { reactLLM = l
              , reactLLMParams = mbParams
              , reactToolList = tools
              }
      let windowMem = WindowBufferMemory 10 (initialChatMessage r)
      let agentState =
            AgentState
              { agentMemory = windowMem
              , agentToolResults = []
              , agentSteps = []
              }
      runAgent reactAgent agentState userQuery

formatTools :: [AnyTool] -> Text
formatTools tools = T.intercalate "\n\n" $ map formatTool tools
  where
    formatTool (AnyTool tool _ _) =
      T.concat ["Tool: ", toolName tool, "\nDescription: ", toolDescription tool]

formatToolNames :: [AnyTool] -> Text
formatToolNames tools = T.intercalate ", " $ map (\(AnyTool tool _ _) -> toolName tool) tools

instance (LLM llm) => Agent (ReactAgent llm) where
  planNextAction ReactAgent {..} AgentState {..} = do
    eMsgs <- messages agentMemory
    case eMsgs of
      Left err -> pure $ Left err
      Right msgs -> do
        eResponse <- chat reactLLM msgs reactLLMParams
        case eResponse of
          Left err -> pure $ Left err
          Right response -> do
            case parse $ content response of
              Left err -> pure (Left $ "Failed to parse response " <> err <> show response)
              Right (ReactAgentOutputParser step) -> return $ Right step

  agentPrompt _ = pure $ PromptTemplate ""
  agentTools ReactAgent {..} = pure reactToolList
