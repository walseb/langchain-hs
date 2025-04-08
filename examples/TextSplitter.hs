
{-# LANGUAGE OverloadedStrings #-}

module TextSplitter where

import Langchain.TextSplitter.Character (splitText, defaultCharacterSplitterOps)
import Data.Text (Text)
import qualified Data.Text as T

main :: IO ()
main = do
  putStrLn "=== Text Splitter Example ==="
  let longText = "This is a long text. It should be split into chunks. \n\nHere is a new paragraph. It continues with more text."
      chunks = splitText defaultCharacterSplitterOps (T.pack longText)
  putStrLn "Text chunks:"
  mapM_ (putStrLn . T.unpack) chunks
