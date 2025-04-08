
{-# LANGUAGE OverloadedStrings #-}

module Agent where

import Langchain.Agents.React
import Langchain.Agents.Core
import Langchain.LLM.Ollama (Ollama(..))
import Langchain.Memory.Core
import Langchain.PromptTemplate (PromptTemplate(..))
import Langchain.LLM.Core (Message(..), Role(..), defaultMessageData)
import qualified Data.Text as T
import qualified Data.List.NonEmpty as NE

main :: IO ()
main = do
  putStrLn "=== Agent Example using ReactAgent with Ollama (llama3.2) ==="
  -- Create an instance of Ollama with the llama3.2 model.
  let ollamaLLM = Ollama "llama3.2" []  -- Provide callbacks if needed.
      tools = []  -- You can add real tools to this list if available.
  agentE <- createReactAgent ollamaLLM tools
  case agentE of
    Left err -> putStrLn $ "Error creating agent: " ++ err
    Right reactAgent -> do
      -- Create initial memory with a system message.
      let initMsg = initialChatMessage "You are an AI assistant powered by Ollama (llama3.2)"
          initMemory = WindowBufferMemory { maxWindowSize = 5, windowBufferMessages = initMsg }
          initState = AgentState { agentMemory = initMemory, agentToolResults = [], agentSteps = [] }
      
      -- Run a single agent step with a sample user query.
      resStep <- runSingleStep reactAgent initState
      putStrLn "Agent step result:"
      print resStep
