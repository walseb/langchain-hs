{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Data.List.NonEmpty as NE

-- Import the ReactAgent creation and output parser.
import Langchain.Agents.React (ReactAgent(..), createReactAgent, ReactAgentOutputParser(..))
import Langchain.Agents.Core
-- Import LLM-related types.
import Langchain.LLM.Core (LLM(..), Message(..), Role(..), defaultMessageData)
-- Import the Tool typeclass and wrapper.
import Langchain.Tool.Core (Tool(..))
-- Import memory types.
import Langchain.Memory.Core (BaseMemory(..), WindowBufferMemory(..))
-- Import the Ollama LLM (make sure this module contains your Ollama instance as provided).
import qualified Langchain.LLM.Ollama as Ollama
-- Import the Wikipedia tool.
import Langchain.Tool.WikipediaTool (WikipediaTool(..), defaultWikipediaTool)

--------------------------------------------------------------------------------
-- Main Function: Using OllamaLLM and the Wikipedia Tool
--------------------------------------------------------------------------------

main :: IO ()
main = do
  -- Create an instance of Ollama LLM with a model name and an empty callback list.
  let ollamaLLM = Ollama.Ollama "deepseek-r1:14b" []
  
  -- Instantiate the Wikipedia tool with its default settings.
  let wikiTool = defaultWikipediaTool
  
  -- Create a list of tools. Wrap the Wikipedia tool using the AnyTool wrapper.
  let tools = [AnyTool wikiTool id id]
  
  -- Create the ReactAgent with the Ollama LLM and the list of tools.
  reactAgentResult <- createReactAgent ollamaLLM tools
  case reactAgentResult of
    Left err -> putStrLn $ "Error creating ReactAgent: " ++ err
    Right reactAgent -> do
      putStrLn "ReactAgent created successfully."
      
      -- Initialize the conversation memory with a system message.
      let initialMessage = Message System "You are an AI assistant." defaultMessageData
          chatHistory   = NE.singleton initialMessage
          memory        = WindowBufferMemory { maxWindowSize = 10, windowBufferMessages = chatHistory }
      
      -- Create a dummy agent state.
      let agentState = AgentState { agentMemory = memory, agentToolResults = [], agentSteps = [] }
      
      -- Simulate a conversation by adding a user message that requests information.
      _ <- addUserMessage memory "Tell me about Haskell programming."
      
      -- Ask the agent to plan its next action based on the updated memory.
      planResult <- planNextAction reactAgent agentState
      case planResult of
        Left err -> putStrLn $ "Error planning action: " ++ err
        Right agentStep -> do
          putStrLn "Agent planned the following step:"
          print agentStep
