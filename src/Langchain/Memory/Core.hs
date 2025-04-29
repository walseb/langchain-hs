{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : Langchain.Memory.Core
Description : Memory management for LangChain Haskell
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Implementation of LangChain's memory management patterns, providing:

- Chat history tracking with size limits
- Message addition/trimming strategies
- Integration with Runnable workflows

Example usage:

@
-- Create memory with 5-message window
memory = WindowBufferMemory 5 (initialChatMessage "You are an assistant")

-- Add user message
newMemory <- addUserMessage memory "Hello, world!"

-- Retrieve current messages
messages <- messages newMemory
-- Right [Message System "...", Message User "Hello, world!"]
@
-}
module Langchain.Memory.Core
  ( BaseMemory (..)
  , WindowBufferMemory (..)
  , trimChatMessage
  , addAndTrim
  , initialChatMessage
  ) where

import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import Langchain.LLM.Core (ChatMessage, Message (..), Role (..), defaultMessageData)
import Langchain.Runnable.Core

{- | Base typeclass for memory implementations
Defines standard operations for chat history management.

Example instance:

@
instance BaseMemory MyMemory where
  messages = ...
  addUserMessage = ...
@
-}
class BaseMemory m where
  -- | Retrieve current chat history
  messages :: m -> IO (Either String ChatMessage)

  -- | Add user message to history
  addUserMessage :: m -> Text -> IO (Either String m)

  -- | Add AI response to history
  addAiMessage :: m -> Text -> IO (Either String m)

  -- | Add generic message to history
  addMessage :: m -> Message -> IO (Either String m)

  -- | Reset memory to initial state
  clear :: m -> IO (Either String m)

{- | Sliding window memory implementation.
Stores chat history with maximum size limit.

Example:

>>> let mem = WindowBufferMemory 2 (NE.singleton (Message System "Sys" defaultMessageData))
>>> addMessage mem (Message User "Hello" defaultMessageData)
Right (WindowBufferMemory {maxWindowSize = 2, ...})
-}
data WindowBufferMemory = WindowBufferMemory
  { maxWindowSize :: Int
  -- ^ Maximum number of messages to retain
  , windowBufferMessages :: ChatMessage
  -- ^ Current message buffer [[9]]
  }
  deriving (Show, Eq)

instance BaseMemory WindowBufferMemory where
  -- \| Get current messages
  --
  --  Example:
  --
  --  >>> messages (WindowBufferMemory 5 initialMessages)
  --  Right initialMessages
  --
  messages WindowBufferMemory {..} = pure $ Right windowBufferMessages

  -- \| Add message with window trimming
  --
  --  Example:
  --
  --  >>> let mem = WindowBufferMemory 2 (NE.fromList [msg1])
  --  >>> addMessage mem msg2
  --  Right (WindowBufferMemory {windowBufferMessages = [msg1, msg2]})
  --
  --  >>> addMessage mem msg3
  --  Right (WindowBufferMemory {windowBufferMessages = [msg2, msg3]})
  --
  addMessage winBuffMem@WindowBufferMemory {..} msg =
    let currentLength = NE.length windowBufferMessages
     in if currentLength >= maxWindowSize
          then
            pure $
              Right $
                winBuffMem
                  { windowBufferMessages =
                      NE.fromList $ (NE.tail windowBufferMessages) ++ [msg]
                  }
          else
            pure $
              Right $
                winBuffMem
                  { windowBufferMessages =
                      windowBufferMessages <> NE.singleton msg
                  }

  -- \| Add user message
  --
  --  Example:
  --
  --  >>> addUserMessage mem "Hello"
  --  Right (WindowBufferMemory { ... })
  --
  addUserMessage winBuffMem uMsg =
    addMessage winBuffMem (Message User uMsg defaultMessageData)

  -- \| Add AI message
  --
  --  Example:
  --
  --  >>> addAiMessage mem "Response"
  --  Right (WindowBufferMemory { ... })
  --
  addAiMessage winBuffMem uMsg =
    addMessage winBuffMem (Message Assistant uMsg defaultMessageData)

  -- \| Reset to initial system message
  --
  --  Example:
  --
  --  >>> clear mem
  --  Right (WindowBufferMemory { windowBufferMessages = [systemMsg] })
  --
  clear winBuffMem =
    pure $
      Right $
        winBuffMem
          { windowBufferMessages =
              NE.singleton $ Message System "You are an AI model" defaultMessageData
          }

{- | Trim chat history to last n messages
Example:

>>> let msgs = NE.fromList [msg1, msg2, msg3]
>>> trimChatMessage 2 msgs
[msg2, msg3]
-}
trimChatMessage :: Int -> ChatMessage -> ChatMessage
trimChatMessage n msgs = NE.fromList $ drop (max 0 (NE.length msgs - n)) (NE.toList msgs)

{- | Add and maintain window size
Example:

>>> let msgs = NE.fromList [msg1]
>>> addAndTrim 2 msg2 msgs
[msg1, msg2]
-}
addAndTrim :: Int -> Message -> ChatMessage -> ChatMessage
addAndTrim n msg msgs = trimChatMessage n (msgs <> NE.singleton msg)

{- | Create initial chat history
Example:

>>> initialChatMessage "You are Qwen"
[Message System "You are Qwen"]
-}
initialChatMessage :: Text -> ChatMessage
initialChatMessage systemPrompt = NE.singleton $ Message System systemPrompt defaultMessageData

instance Runnable WindowBufferMemory where
  type RunnableInput WindowBufferMemory = Text
  type RunnableOutput WindowBufferMemory = WindowBufferMemory

  -- \| Runnable interface for user input
  --
  --  Example:
  --
  --  >>> invoke memory "Hello"
  --  Right (WindowBufferMemory { ... })
  --
  invoke memory input = addUserMessage memory input

{- $examples
Test case patterns:
1. Message trimming
   >>> let mem = WindowBufferMemory 2 [msg1, msg2]
   >>> addMessage mem msg3
   Right [msg2, msg3]

2. Initial state
   >>> messages (WindowBufferMemory 5 initialMessages)
   Right initialMessages

3. Runnable integration
   >>> run (WindowBufferMemory 5 initialMessages) "Hello"
   Right (WindowBufferMemory { ... })
-}
