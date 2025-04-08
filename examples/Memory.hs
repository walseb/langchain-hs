
{-# LANGUAGE OverloadedStrings #-}

module Memory where

import Langchain.Memory.Core
import Langchain.LLM.Core (Message(..), Role(..), defaultMessageData)
import qualified Data.Text as T
import qualified Data.List.NonEmpty as NE

main :: IO ()
main = do
  putStrLn "=== Memory Example with WindowBufferMemory ==="

  -- Create an initial system message and memory.
  let initialMsg = Message System "You are an AI model" defaultMessageData
      memory = WindowBufferMemory { maxWindowSize = 3, windowBufferMessages = NE.singleton initialMsg }

  -- Add a user message.
  result1 <- addUserMessage memory "Hello, how are you?"
  case result1 of
    Left err    -> putStrLn $ "Error: " ++ err
    Right newMem -> do
      putStrLn "After adding user message:"
      msgs <- messages newMem
      print msgs

      -- Add an AI message.
      result2 <- addAiMessage newMem "I am doing well, thanks!"
      case result2 of
        Left err  -> putStrLn $ "Error: " ++ err
        Right mem2 -> do
          putStrLn "After adding AI message:"
          msgs2 <- messages mem2
          print msgs2
