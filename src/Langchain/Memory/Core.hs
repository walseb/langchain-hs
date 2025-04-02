{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Langchain.Memory.Core (
      BaseMemory (..)
    , WindowBufferMemory (..)
    , trimChatMessage
    , addAndTrim
    , initialChatMessage
) where

import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import Langchain.LLM.Core (ChatMessage, Message (..), Role (..), defaultMessageData)

class BaseMemory m where
  messages :: m -> IO (Either String ChatMessage)
  addUserMessage :: m -> Text -> IO (Either String m)
  addAiMessage :: m -> Text -> IO (Either String m)
  addMessage :: m -> Message -> IO (Either String m)
  clear :: m -> IO (Either String m)

data WindowBufferMemory = WindowBufferMemory
  { maxWindowSize :: Int
  , windowBufferMessages :: ChatMessage
  }
  deriving (Show, Eq)

instance BaseMemory WindowBufferMemory where
  messages WindowBufferMemory {..} = pure $ Right windowBufferMessages

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
                      windowBufferMessages `NE.append` NE.singleton msg
                  }

  addUserMessage winBuffMem uMsg =
    addMessage winBuffMem (Message User uMsg defaultMessageData)

  addAiMessage winBuffMem uMsg =
    addMessage winBuffMem (Message Assistant uMsg defaultMessageData)

  clear winBuffMem =
    pure $
      Right $
        winBuffMem
          { windowBufferMessages =
              NE.singleton $ Message System "You are an AI model" defaultMessageData
          }

-- | Trim the chat history to the last n messages.
-- If n is larger than or equal to the current number of messages, returns the entire chat history.
trimChatMessage :: Int -> ChatMessage -> ChatMessage
trimChatMessage n msgs = NE.fromList $ drop (max 0 (NE.length msgs - n)) (NE.toList msgs)

-- | Add a message to the chat history and trim to the last n messages.
addAndTrim :: Int -> Message -> ChatMessage -> ChatMessage
addAndTrim n msg msgs = trimChatMessage n (msgs `NE.append` NE.singleton msg)

-- | Create an initial chat history with a system message.
initialChatMessage :: Text -> ChatMessage
initialChatMessage systemPrompt = NE.singleton $ Message System systemPrompt defaultMessageData
