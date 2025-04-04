{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Langchain.Runnable.ConversationChain (ConversationChain (..)) where

import Data.Text (Text)
import Langchain.LLM.Core
import Langchain.Memory.Core
import Langchain.PromptTemplate
import Langchain.Runnable.Core

-- | ConversationChain
data ConversationChain m l = ConversationChain
  { memory :: m
  , llm :: l
  , prompt :: PromptTemplate
  }

-- | Make ConversationChain an instance of Runnable
instance (BaseMemory m, LLM l) => Runnable (ConversationChain m l) where
  type RunnableInput (ConversationChain m l) = Text
  type RunnableOutput (ConversationChain m l) = Text

  invoke ConversationChain {..} input = do
    -- Add user message to memory
    updatedMemResult <- addUserMessage memory input
    case updatedMemResult of
      Left err -> return $ Left err
      Right updatedMem -> do
        -- Get all messages
        messagesResult <- messages updatedMem
        case messagesResult of
          Left err -> return $ Left err
          Right allMessages -> do
            -- Format messages for the LLM
            let formattedMessages = allMessages
            -- Get response from LLM
            llmResponse <- chat llm formattedMessages Nothing
            case llmResponse of
              Left err -> return $ Left err
              Right response -> do
                -- Store AI response in memory
                _ <- addAiMessage updatedMem response
                return $ Right response
