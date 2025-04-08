
{-# LANGUAGE OverloadedStrings #-}

module DocumentLoader where

import Langchain.DocumentLoader.Core (Document(..), BaseLoader(..))
import Langchain.DocumentLoader.FileLoader (FileLoader(..))
import Langchain.DocumentLoader.PdfLoader (PdfLoader(..))
import qualified Data.Text as T

main :: IO ()
main = do
  putStrLn "=== Document Loader Examples ==="
  
  -- FileLoader example: Ensure that "test.txt" exists in your working directory.
  let fileLoader = FileLoader "test.txt"
  fileResult <- load fileLoader
  case fileResult of
    Left err  -> putStrLn $ "FileLoader error: " ++ err
    Right docs -> do
      putStrLn "FileLoader loaded documents:"
      mapM_ (print . pageContent) docs

  -- PdfLoader example: Ensure that "sample.pdf" exists in your working directory.
  let pdfLoader = PdfLoader "sample.pdf"
  pdfResult <- load pdfLoader
  case pdfResult of
    Left err  -> putStrLn $ "PdfLoader error: " ++ err
    Right docs -> do
      putStrLn "PdfLoader loaded documents:"
      mapM_ (print . pageContent) docs
