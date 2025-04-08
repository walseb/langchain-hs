{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module OutputParser where

import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Langchain.OutputParser.Core
import Data.Aeson (FromJSON)

-- A simple data type to demonstrate JSON parsing.
data Person = Person 
  { name :: Text
  , age  :: Int 
  } deriving (Show, Eq, Generic)

instance FromJSON Person

main :: IO ()
main = do
  putStrLn "=== Output Parser Examples ==="
  print (parse "true"       :: Either String Bool)
  print (parse "False"      :: Either String Bool)
  print (parse "yes"        :: Either String Bool)

  -- Comma-separated list parsing example
  let csvExample = "apple, banana, cherry"
  print (parse csvExample   :: Either String CommaSeparatedList)

  -- Number-separated list parsing example
  let nslExample = "1. First item\n2. Second item\n3. Third item"
  print (parse nslExample   :: Either String NumberSeparatedList)

  -- JSON output structure parsing example
  let jsonExample = "{\"name\": \"Bob\", \"age\": 25}"
  print (parse jsonExample  :: Either String (JSONOutputStructure Person))
