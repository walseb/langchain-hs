{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Langchain.Memory.Core (BaseMemory (..), WindowBufferMemory (..)) where

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
