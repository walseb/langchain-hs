module Main (main) where

-- import qualified App.OpenRouter.PdfQA as PdfQA
import qualified App.OpenRouter.ToolCall as ToolCall

main :: IO ()
main = ToolCall.runApp
