{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : Langchain.Agents.Core
Description : Core implementation of LangChain agents
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>

Agents use LLMs as reasoning engines to determine actions dynamically
This module implements the core agent execution loop and interfaces,
supporting tool interaction and memory management.

Example agent execution flow:

> executor <- AgentExecutor
>   { executor = myAgent
>   , executorMemory = emptyMemory
>   , maxIterations = 5
>   , returnIntermediateSteps = True
>   }
> result <- runAgentExecutor executor "Explain quantum computing"
-}
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
import qualified Langchain.Runnable.Core as Run
import Langchain.Tool.Core (Tool (..))

{- |
Represents an action to be taken by the agent
-}
data AgentAction = AgentAction
  { actionToolName :: Text
  -- ^ Tool name
  , actionInput :: Text
  -- ^ Input
  , actionLog :: Text
  -- ^ Execution log
  }
  deriving (Eq, Show)

-- | Represents that agent has finished work with final value
data AgentFinish = AgentFinish
  { returnValues :: Map.Map Text Text 
  , finishLog :: Text
  }
  deriving (Show, Eq)

-- | Type that will be return from LLM 
-- Could be either Continue, making another call to LLM or Finish with final value
data AgentStep
  = Continue AgentAction
  | Finish AgentFinish
  deriving (Eq, Show)

-- | Type for maintaining state of the agent 
data (BaseMemory m) => AgentState m = AgentState
  { agentMemory :: m -- ^ Memory for storing chat history
  , agentToolResults :: [(Text, Text)] -- ^ Tool results
  , agentSteps :: [AgentAction] -- ^ Agent steps happened so far
  }
  deriving (Eq, Show)

{- |
Dynamic tool wrapper allowing heterogeneous tool collections
Converts between Text and tool-specific input/output types.

Example usage:

> calculatorTool :: AnyTool
> calculatorTool = customAnyTool
>   Calculator
>   (\t -> read (T.unpack t) :: (Int, Int))
>   (T.pack . show)
-}
data AnyTool = forall a. Tool a => AnyTool
  { anyTool :: a
  , textToInput :: Text -> Input a
  , outputToText :: Output a -> Text
  }

{- |
Core agent class defining required operations

* Plan next action based on state
* Provide prompt template
* Expose available tools
-}
class Agent a where
  planNextAction :: BaseMemory m => a -> AgentState m -> IO (Either String AgentStep)
  agentPrompt :: a -> IO PromptTemplate
  agentTools :: a -> IO [AnyTool]

{- |
Agent execution engine
-}
data AgentExecutor a m = AgentExecutor
  { executor :: a -- Agent instance
  , executorMemory :: m
  -- ^ Memory state
  , maxIterations :: Int
  -- ^ Iteration limits
  , returnIntermediateSteps :: Bool
  -- ^ Step tracking
  }
  deriving (Eq, Show)

{- |
Run the full agent execution loop
Handles:

1. Memory updates
2. Action planning
3. Tool execution
4. Iteration control

Example flow:

1. User input -> memory
2. Plan action -> execute tool
3. Store result -> memory
4. Repeat until finish

Throws errors for:

- Tool not found [[5]]
- Execution errors
- Iteration limits
-}
runAgent :: (Agent a, BaseMemory m) => a -> AgentState m -> Text -> IO (Either String AgentFinish)
runAgent agent initialState@AgentState {..} initialInput = do
  memWithInput <- addUserMessage agentMemory initialInput
  case memWithInput of
    Left err -> return $ Left err
    Right updatedMem ->
      let newState = initialState {agentMemory = updatedMem}
       in runAgentLoop agent newState 0 10

-- | Helper function for runAgent
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

-- | Alias for planNextAction
runSingleStep :: (Agent a, BaseMemory m) => a -> AgentState m -> IO (Either String AgentStep)
runSingleStep = planNextAction

{- |
Execute a single tool call
Handles tool lookup and input/output conversion.

Example:

> tools = [calculatorTool, wikipediaTool]
> executeTool tools "calculator" "(5, 3)"
> -- Returns Right "8"
-}
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

{- |
Helper for creating custom tool wrappers
Requires conversion functions between Text and tool-specific types.

Example:

> weatherTool = customAnyTool
>   WeatherAPI
>   parseLocation
>   formatWeatherResponse
-}
customAnyTool :: Tool a => a -> (Text -> Input a) -> (Output a -> Text) -> AnyTool
customAnyTool tool inputConv outputConv = AnyTool tool inputConv outputConv

-- | Similar to runAgent, but for AgentExecutor
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

{- |
Runnable instance for agent execution
Allows integration with LangChain workflows.

Example:

> response <- invoke myAgentExecutor "Solve 5+3"
> case response of
>   Right result -> print result
>   Left err -> print err
-}
instance (Agent a, BaseMemory m) => Run.Runnable (AgentExecutor a m) where
  type RunnableInput (AgentExecutor a m) = Text
  type RunnableOutput (AgentExecutor a m) = AgentFinish

  invoke AgentExecutor {..} input = do
    let initialState =
          AgentState
            { agentMemory = executorMemory
            , agentToolResults = []
            , agentSteps = []
            }
    runAgent executor initialState input
