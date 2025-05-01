{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : Langchain.Memory.TokenBufferMemory
Description : Token based Memory management for LangChain Haskell
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Implementation of LangChain's Conversation token buffer.
https://python.langchain.com/v0.1/docs/modules/memory/types/token_buffer/
-}
module Langchain.Memory.TokenBufferMemory (TokenBufferMemory (..), countTokens) where

import Data.List (uncons)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Langchain.LLM.Core (ChatMessage, Message (..), Role (..), defaultMessageData)
import Langchain.Memory.Core
import Langchain.Runnable.Core (Runnable(..))

-- | Token based sliding window memory type
data TokenBufferMemory = TokenBufferMemory
  { maxTokens :: Int -- ^ Max number of tokens. 4 characters = 1 Token
  , tokenBufferMessages :: ChatMessage -- ^ Chat history (Nonempty List of Message)
  }
  deriving (Eq, Show)

-- | Function for counting tokens for the given list of messages
-- | 1 token = 4 characters
countTokens :: [Message] -> Int
countTokens = sum . map go
  where
    go :: Message -> Int
    go (Message _ content _) = ceiling @Double (fromIntegral (T.length content) / 4.0)

instance BaseMemory TokenBufferMemory where
  messages TokenBufferMemory {..} = pure $ Right tokenBufferMessages
  addMessage t@TokenBufferMemory {..} newMsg = do
    let newTokenBuffMsgs = NE.toList tokenBufferMessages <> [newMsg]
     in if countTokens newTokenBuffMsgs <= maxTokens
          then
            pure $
              Right $
                t
                  { tokenBufferMessages =
                      NE.fromList newTokenBuffMsgs
                  }
          else
            let trimMsgs msgs =
                  case uncons msgs of
                    Nothing -> []
                    Just (_, withoutOldest) ->
                      let newMsgTokens = countTokens [newMsg]
                          withoutOldestTokens = countTokens withoutOldest
                       in if newMsgTokens + withoutOldestTokens <= maxTokens
                            then
                              withoutOldest
                            else trimMsgs withoutOldest
                trimmedMsgs = trimMsgs (NE.toList tokenBufferMessages)
             in if null trimmedMsgs
                  then
                    -- Edge case: new message alone exceeds the limit
                    pure $
                      Left "New message is exceeding limit"
                  else
                    pure $
                      Right $
                        t
                          { tokenBufferMessages =
                              NE.fromList $ trimmedMsgs ++ [newMsg]
                          }
  addUserMessage tokBuffMem uMsg =
    addMessage tokBuffMem (Message User uMsg defaultMessageData)

  addAiMessage tokBuffMem uMsg =
    addMessage tokBuffMem (Message Assistant uMsg defaultMessageData)

  clear tokBuffMem =
    pure $
      Right $
        tokBuffMem
          { tokenBufferMessages =
              NE.singleton $ Message System "You are an AI model" defaultMessageData
          }

instance Runnable TokenBufferMemory where
  type RunnableInput TokenBufferMemory = T.Text
  type RunnableOutput TokenBufferMemory = TokenBufferMemory

  invoke memory input = addUserMessage memory input
