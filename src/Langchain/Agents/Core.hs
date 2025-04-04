{- |
Module      : Langchain.Agents.Core
Description : Implementation of agents that use LLMs to determine actions
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com

This module provides the core functionality for agents in LangChain.
Agents use language models to determine which actions to take and in what order.
-}

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Langchain.Agents.Core
  ( AgentAction (..)
  , AgentFinish (..)
  , AgentStep (..)
  , Agent (..)
  , AnyTool (..)
  , AgentState (..)
  , AgentExecutor (..)
  , runAgent
  , runAgentLoop
  , runAgentExecutor
  , executeTool
  , runSingleStep
  , customAnyTool
  ) where

import Control.Exception (SomeException, try)
import Data.List (find)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Langchain.LLM.Core (Message (Message), Role (..), defaultMessageData)
import Langchain.Memory.Core (BaseMemory (..))
import Langchain.PromptTemplate (PromptTemplate)
import Langchain.Tool.Core (Tool (..))
import qualified Langchain.Runnable.Core as Run

data AgentAction = AgentAction
  { actionToolName :: Text
  , actionInput :: Text
  , actionLog :: Text
  }
  deriving (Eq, Show)

data AgentFinish = AgentFinish
  { returnValues :: Map.Map Text Text
  , finishLog :: Text
  }
  deriving (Show, Eq)

data AgentStep
  = Continue AgentAction
  | Finish AgentFinish
  deriving (Eq, Show)

data (BaseMemory m) => AgentState m = AgentState
  { agentMemory :: m
  , agentToolResults :: [(Text, Text)]
  , agentSteps :: [AgentAction]
  }
  deriving (Eq, Show)

-- | Wrapper for tools with conversion functions for inputs and outputs
data AnyTool = forall a. Tool a => AnyTool
  { anyTool :: a
  , textToInput :: Text -> Input a
  , outputToText :: Output a -> Text
  }

class Agent a where
  planNextAction :: BaseMemory m => a -> AgentState m -> IO (Either String AgentStep)
  agentPrompt :: a -> IO PromptTemplate
  agentTools :: a -> IO [AnyTool]

data AgentExecutor a m = AgentExecutor
  { executor :: a -- Agent
  , executorMemory :: m
  , maxIterations :: Int
  , returnIntermediateSteps :: Bool
  }
  deriving (Eq, Show)

runAgent :: (Agent a, BaseMemory m) => a -> AgentState m -> Text -> IO (Either String AgentFinish)
runAgent agent initialState@AgentState {..} initialInput = do
  memWithInput <- addUserMessage agentMemory initialInput
  case memWithInput of
    Left err -> return $ Left err
    Right updatedMem ->
      let newState = initialState {agentMemory = updatedMem}
       in runAgentLoop agent newState 0 10

runAgentLoop ::
  (Agent a, BaseMemory m) => a -> AgentState m -> Int -> Int -> IO (Either String AgentFinish)
runAgentLoop agent agentState@AgentState {..} currIter maxIter
  | currIter > maxIter = return $ Left "Max iterations excedded"
  | otherwise = do
      eStepResult <- runSingleStep agent agentState
      case eStepResult of
        Left err -> return $ Left err
        Right (Finish agentFinish) -> return $ Right agentFinish
        Right (Continue act@AgentAction {..}) -> do
          toolList <- agentTools agent
          toolResult <- executeTool toolList actionToolName actionInput
          case toolResult of
            Left err -> return $ Left err
            Right result -> do
              -- Add the tool result to memory as a tool message
              let toolMsg = Message Tool result defaultMessageData
              updatedMemResult <- addMessage agentMemory toolMsg
              case updatedMemResult of
                Left err -> return $ Left err
                Right updatedMem ->
                  let updatedState =
                        agentState
                          { agentMemory = updatedMem
                          , agentToolResults = agentToolResults ++ [(actionToolName, result)]
                          , agentSteps = agentSteps ++ [act]
                          }
                   in runAgentLoop agent updatedState (currIter + 1) maxIter

runSingleStep :: (Agent a, BaseMemory m) => a -> AgentState m -> IO (Either String AgentStep)
runSingleStep = planNextAction

executeTool :: [AnyTool] -> Text -> Text -> IO (Either String Text)
executeTool tools toolName_ input =
  case find (\(AnyTool t _ _) -> toolName t == toolName_) tools of
    Nothing -> return $ Left $ "Tool not found: " <> T.unpack toolName_
    Just (AnyTool {..}) -> do
      resultE <- try $ do
        let typedInput = textToInput input
        result <- runTool anyTool typedInput
        return $ outputToText result
      case resultE of
        Left ex -> return $ Left $ "Tool execution error: " <> show (ex :: SomeException)
        Right output -> return $ Right output

-- | Helper to create a fully custom AnyTool with conversion functions
customAnyTool :: Tool a => a -> (Text -> Input a) -> (Output a -> Text) -> AnyTool
customAnyTool tool inputConv outputConv = AnyTool tool inputConv outputConv

-- | Run an agent executor with the given input
runAgentExecutor ::
  (Agent a, BaseMemory m) => AgentExecutor a m -> Text -> IO (Either String (Maybe AgentFinish))
runAgentExecutor AgentExecutor {..} input = do
  let initialState =
        AgentState
          { agentMemory = executorMemory
          , agentToolResults = []
          , agentSteps = []
          }
  result <- runAgent executor initialState input
  case result of
    Left err -> return $ Left err
    Right a ->
      if returnIntermediateSteps
        then return $ Right $ Just a
        else return $ Right Nothing

-- | Make Agent a Runnable
instance (Agent a, BaseMemory m) => Run.Runnable (AgentExecutor a m) where
  type RunnableInput (AgentExecutor a m) = Text
  type RunnableOutput (AgentExecutor a m) = AgentFinish
  
  invoke AgentExecutor{..} input = do
    let initialState = AgentState
          { agentMemory = executorMemory
          , agentToolResults = []
          , agentSteps = []
          }
    runAgent executor initialState input