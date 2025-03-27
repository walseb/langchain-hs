{-# LANGUAGE OverloadedStrings #-}

module Langchain.DocumentLoader.FileLoader (
    FileLoader (..)
  ) where

import Langchain.DocumentLoader.Core
import Data.Text (pack)
import Data.Map (fromList)
import Data.Aeson 
import System.Directory (doesFileExist)
import Langchain.TextSplitter.RecursiveCharacter

-- | A loader for text files.
data FileLoader = FileLoader FilePath

instance BaseLoader FileLoader where

    load (FileLoader path) = do
        exists <- doesFileExist path
        if exists
            then do
                content <- readFile path
                let meta = fromList [("source", String $ pack path)]
                return $ Right [Document (pack content) meta]
            else
                return $ Left $ "File not found: " ++ path

    loadAndSplit (FileLoader path) = do
        exists <- doesFileExist path
        if exists
            then do
                content <- readFile path
                return $ Right $ splitText defaultRecursiveCharacterOptions (pack content)
            else
                return $ Left $ "File not found: " ++ path
