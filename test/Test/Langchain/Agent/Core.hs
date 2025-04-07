{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Langchain.Agent.Core where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, assertBool, (@?=))
import Control.Exception (throwIO)
import Data.IORef (newIORef, readIORef, writeIORef, IORef)
import qualified Data.Map.Strict as Map
import Data.Text (Text, pack, isInfixOf)
import Data.Char (toUpper)
import Langchain.Agents.Core
import Langchain.LLM.Core
import Langchain.Memory.Core (BaseMemory(..))
import Langchain.Tool.Core (Tool(..))
import Langchain.Runnable.Core (invoke)
import Langchain.PromptTemplate
import qualified Data.List.NonEmpty as NE

-- Mock Tool Implementations

data DummyTool = DummyTool deriving (Show)

instance Tool DummyTool where
  type Input DummyTool = Text
  type Output DummyTool = Text
  toolName _ = "dummy-tool"
  runTool _ input = return $ "Processed: " <> input

data FaultyTool = FaultyTool deriving (Show)

instance Tool FaultyTool where
  type Input FaultyTool = Text
  type Output FaultyTool = Text
  toolName _ = "faulty-tool"
  runTool _ _ = throwIO $ userError "Intentional tool error"

-- Mock Agent with Step Sequence

data StepSequenceAgent = StepSequenceAgent (IORef [AgentStep]) [AnyTool]

instance Agent StepSequenceAgent where
  planNextAction (StepSequenceAgent ref _) _ = do
    steps <- readIORef ref
    case steps of
      [] -> return $ Left "No steps left"
      (step:rest) -> do
        writeIORef ref rest
        return $ Right step
  agentTools (StepSequenceAgent _ tools) = return tools
  agentPrompt _ = return $ PromptTemplate "test prompt"

-- Test Memory Implementation

data TestMemory = TestMemory [Message]

instance BaseMemory TestMemory where
  addMessage (TestMemory msgs) newMsg = return $ Right $ TestMemory (msgs ++ [newMsg])
  addUserMessage (TestMemory msgs) input = do
    let userMsg = Message User input defaultMessageData
    return $ Right $ TestMemory (msgs ++ [userMsg])
  messages (TestMemory msgs) = return $ Right $ NE.fromList msgs

-- Test Cases

tests :: TestTree
tests = testGroup "Agent Tests"
  [ testCase "executeTool valid tool" $ do
      let dummyAnyTool = customAnyTool DummyTool id id
          tools = [dummyAnyTool]
      result <- executeTool tools "dummy-tool" "test input"
      assertEqual "Should process input" (Right "Processed: test input") result

  , testCase "executeTool tool not found" $ do
      let tools = []
      result <- executeTool tools "unknown-tool" "input"
      assertEqual "Should return tool not found error" (Left "Tool not found: unknown-tool") result

  , testCase "executeTool tool throws exception" $ do
      let faultyAnyTool = customAnyTool FaultyTool id id
          tools = [faultyAnyTool]
      result <- executeTool tools "faulty-tool" "input"
      assertBool "Should return execution error" ("Intentional tool error" `isInfixOf` (pack $ fromLeft "" result))

  , testCase "runAgentLoop max iterations exceeded" $ do
      agentRef <- newIORef []
      let agent = StepSequenceAgent agentRef []
          initialState = AgentState (TestMemory []) [] []
      result <- runAgentLoop agent initialState 10 5
      assertEqual "Should return max iteration error" (Left "Max iterations excedded") result

  , testCase "runAgent immediate finish" $ do
      agentRef <- newIORef [Finish (AgentFinish (Map.singleton "result" "success") "Finished")]
      let agent = StepSequenceAgent agentRef []
          initialState = AgentState (TestMemory []) [] []
      result <- runAgent agent initialState "input"
      assertEqual "Should return finish result" (Right (AgentFinish (Map.singleton "result" "success") "Finished")) result

  , testCase "runAgentLoop continue then finish" $ do
      agentRef <- newIORef [ Continue (AgentAction "dummy-tool" "input" "log")
                           , Finish (AgentFinish Map.empty "Done") ]
      let dummyAnyTool = customAnyTool DummyTool id id
          agent = StepSequenceAgent agentRef [dummyAnyTool]
          initialState = AgentState (TestMemory []) [] []
      result <- runAgentLoop agent initialState 0 10
      assertEqual "Should finish after one step" (Right (AgentFinish Map.empty "Done")) result

  , testCase "customAnyTool wraps correctly" $ do
      let tool = customAnyTool DummyTool id id
          input = "test"
          expectedOutput = "PROCESSED: TSET"
      result <- executeTool [tool] "dummy-tool" input
      assertEqual "Should apply conversions" (Right expectedOutput) result
  ]
  where
    fromLeft :: a -> Either a b -> a
    fromLeft def (Left x) = x
    fromLeft def _ = def