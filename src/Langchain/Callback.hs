module Langchain.Callback
  ( Event (..)
  , Callback
  , stdOutCallback
  ) where

data Event
  = LLMStart
  | LLMEnd
  | LLMError String
  deriving (Show, Eq)

type Callback = Event -> IO ()

stdOutCallback :: Callback
stdOutCallback event = case event of
  LLMStart -> putStrLn "Model operation started"
  LLMEnd -> putStrLn $ "Model completed with"
  LLMError err -> putStrLn $ "Error occurred: " ++ err
