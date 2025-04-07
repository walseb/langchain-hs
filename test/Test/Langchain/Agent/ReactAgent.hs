{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Langchain.Agent.ReactAgent (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, assertBool)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.List.NonEmpty as NE
import Langchain.Agents.Core
import Langchain.Agents.React
import Langchain.LLM.Core
import Langchain.Memory.Core (BaseMemory(..))
import Langchain.Tool.Core (Tool(..))

--TODO: Need to fix answering parsing by stripping ": "
data MockLLM = MockLLM { mockResponse :: Text }

instance LLM MockLLM where
  generate _ _ _ = undefined
  chat (MockLLM resp) _ _ = return $ Right resp
  stream _ _ _ _ = undefined

data DummyTool = DummyTool deriving (Show)

instance Tool DummyTool where
  type Input DummyTool = Text
  type Output DummyTool = Text
  toolName _ = "dummy-tool"
  toolDescription _ = "A dummy tool for testing"
  runTool _ input = return $ "Processed: " <> input

data TestMemory = TestMemory [Message]

instance BaseMemory TestMemory where
  addMessage (TestMemory msgs) newMsg = return $ Right $ TestMemory (msgs ++ [newMsg])
  addUserMessage (TestMemory msgs) input = do
    let userMsg = Message User input defaultMessageData
    return $ Right $ TestMemory (msgs ++ [userMsg])
  addAiMessage (TestMemory msgs) input = do
    let aiMsg = Message System input defaultMessageData
    return $ Right $ TestMemory (msgs ++ [aiMsg])
  messages (TestMemory msgs) = return $ return $ NE.fromList msgs
  clear _ = pure $ Right $ TestMemory []

tests :: TestTree
tests = testGroup "React Agent Tests"
  [ testCase "parseReactOutput final answer" $ do
      let input = "Thought: I know the answer\nFinal Answer: Success"
      let result = parseReactOutput input
      case result of
        Right (ReactAgentOutputParser (Finish (AgentFinish vals _))) ->
          assertEqual "Should parse final answer" (Map.singleton "output" ": Success") vals
        _ -> assertBool "Failed to parse final answer" False

  , testCase "parseReactOutput action step" $ do
      let input = "Action: dummy-tool\nAction Input: test input"
      let result = parseReactOutput input
      case result of
        Right (ReactAgentOutputParser (Continue act)) -> do
          assertEqual "Correct tool name" ": dummy-tool" (actionToolName act)
          assertEqual "Correct input" ": test input" (actionInput act)
        _ -> assertBool "Failed to parse action" False

  , testCase "parseReactOutput invalid input" $ do
      let input = "Invalid format"
      let result = parseReactOutput input
      case result of
        Left err -> assertBool "Should return parse error" ("Could not parse" `T.isInfixOf` (T.pack err))
        _ -> assertBool "Should fail on invalid input" False

  , testCase "planNextAction generates action step" $ do
      let llm = MockLLM { mockResponse = "Action: dummy-tool\nAction Input: test" }
      let tools = [customAnyTool DummyTool id id]
      agent <- createReactAgent llm tools
      case agent of
        Left _ -> assertBool "Agent creation failed" False
        Right reactAgent -> do
          let mem = TestMemory [Message User "Solve this" defaultMessageData]
          let state = AgentState mem [] []
          result <- planNextAction reactAgent state
          case result of
            Right (Continue act) -> do
              assertEqual "Correct tool name" ": dummy-tool" (actionToolName act)
              assertEqual "Correct input" ": test" (actionInput act)
            _ -> assertBool "Should generate action step" False

  , testCase "planNextAction final answer" $ do
      let llm = MockLLM { mockResponse = "Final Answer: 42" }
      let tools = []
      agent <- createReactAgent llm tools
      case agent of
        Left _ -> assertBool "Agent creation failed" False
        Right reactAgent -> do
          let mem = TestMemory [Message User "What's the answer?" defaultMessageData]
          let state = AgentState mem [] []
          result <- planNextAction reactAgent state
          case result of
            Right (Finish (AgentFinish vals _)) ->
              assertEqual "Correct final answer" (Map.singleton "output" ": 42") vals
            _ -> assertBool "Should generate final answer" False

  , testCase "createReactAgent prompt formatting" $ do
      let llm = MockLLM { mockResponse = "" }
      let tools = [customAnyTool DummyTool id id]
      agent <- createReactAgent llm tools
      case agent of
        Right ReactAgent {..} -> do
          let expectedTools = "Tool: dummy-tool\nDescription: A dummy tool for testing"
              expectedNames = "dummy-tool"
          assertEqual "Correct tool descriptions" expectedTools (formatToolDescriptions reactTools)
          assertEqual "Correct tool names" expectedNames (formatToolNames reactTools)
        _ -> assertBool "Agent creation failed" False

  , testCase "getLastUserInput retrieves last user message" $ do
      let msgs = NE.fromList [ Message User "First" defaultMessageData
                 , Message Assistant "Response" defaultMessageData
                 , Message User "Last" defaultMessageData ] 
      let result = getLastUserInput msgs
      assertEqual "Should get last user input" "Last" result

  , testCase "getLastUserInput no user messages" $ do
      let msgs = NE.fromList [ Message Assistant "Only" defaultMessageData ] 
      let result = getLastUserInput msgs
      assertEqual "Should return empty" "" result
  ]
  where
    -- isLeft (Left _) = True
    -- isLeft _ = False
