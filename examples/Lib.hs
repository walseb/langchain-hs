{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    ) where

import Data.Either
import Data.Aeson
import Langchain.PromptTemplate
import Langchain.Callback
import Langchain.LLM.Ollama
import Langchain.LLM.Core
import Langchain.DocumentLoader.PdfLoader
import Langchain.DocumentLoader.FileLoader
import Langchain.DocumentLoader.Core
import Langchain.TextSplitter.Character
import Langchain.Embeddings.Ollama
import Langchain.Embeddings.Core
import Langchain.Memory.Core
import Langchain.OutputParser.Core
import Langchain.VectorStore.InMemory
import Langchain.VectorStore.Core
import Langchain.Retriever.Core
import Langchain.Retriever.MultiQueryRetriever
import Langchain.Tool.WikipediaTool
import Langchain.Tool.Core
import Langchain.Agents.React
import Langchain.Agents.Core
import qualified Langchain.Runnable.Core as Chain
import qualified Data.Map.Strict as Map
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.IO as T

-- A simple data type to demonstrate JSON parsing.
data SomeVal = SomeVal {
    age :: Int,
    empId :: String
  } deriving (Eq, Show)

instance FromJSON SomeVal where
  parseJSON (Object v) = 
    SomeVal <$> v .: "age"
            <*> v .: "empId"
  parseJSON _ = error "Expected an object for SomeVal"

someFunc :: IO ()
someFunc = do 
  putStrLn "=== Prompt Template Examples ==="
  -- Normal usage of renderPrompt
  let temp = PromptTemplate "Convert the following text into {language} language.\nText: {text}"
      renderedPrompt = renderPrompt temp $ 
                Map.fromList [("text", "Nice to meet you"), ("language", "French")]
      renderedPromptRunnable = Chain.invoke temp $ 
                Map.fromList [("text", "Nice to meet you"), ("language", "French")]
  print renderedPrompt 
  -- Example of failing usage (missing expected key)
  let renderedPrompt2 = renderPrompt temp $ 
                Map.fromList [("something", "Nice to meet you"), ("language", "French")]
  print renderedPrompt2

  putStrLn "\n=== Few-Shot Prompt Template Example ==="
  let fewShotPromptTemp = FewShotPromptTemplate {
          fsPrefix = "Here are some examples of sentences and their sentiment: "
         , fsExamples = [
                Map.fromList [("sentence", "The movie was good"), ("sentiment", "positive")]
              , Map.fromList [("sentence", "The movie was quite bad"), ("sentiment", "negative")]
              , Map.fromList [
                  ("sentence", "I really like the movie, but the ending was lacking")
                  , ("sentiment", "neutral")]
            ]
         , fsExampleTemplate = "{sentence} : {sentiment} " 
         , fsExampleSeparator = "\n"
         , fsSuffix = "Give me the sentiment for this sentence: The movie was Mid AF!, sentiment = ?"
        }
  let renderedFewshotPrompt = renderFewShotPrompt fewShotPromptTemp
  print renderedFewshotPrompt

  putStrLn "\n=== Chat and Streaming with OllamaLLM ==="
  -- Creating chat messages for demonstration.
  let chatMsg = NE.fromList [Message User "What is the meaning of Life?" defaultMessageData]
      chatMsg2 = NE.fromList [Message User "What is Monad in Haskell?" defaultMessageData]
      chatMsg3 = NE.fromList [Message User "What is Applicative in Haskell?" defaultMessageData]
      streamHandler = StreamHandler {
          onToken = T.putStr,  -- Stream individual tokens to stdout.
          onComplete = pure ()
      }

  -- Create an OllamaLLM instance using the "llama3.2:latest" model.
  let ollamaLLM = Ollama "llama3.2:latest" [stdOutCallback]
  ollamaGenRes <- generate ollamaLLM "What is 2+2?" Nothing
  print ollamaGenRes
  
  ollamaChatRes <- chat ollamaLLM chatMsg (Just $ defaultParams { temperature = Just 0.5 })
  print ollamaChatRes

  -- Streaming response example.
  ollamaStreamRes <- stream ollamaLLM chatMsg2 streamHandler Nothing
  print ollamaStreamRes

  -- Using the runnable (chain) interface for chat.
  chainedRes <- Chain.invoke ollamaLLM chatMsg3
  print chainedRes

  putStrLn "\n=== Document Loader Examples ==="
  -- Document loader: PDF and file-based loaders.
  let pdfFilePath = PdfLoader "/home/user/Documents/TS/langchain/SOP.pdf"
      fileLoaderPath = FileLoader "/home/user/Documents/TS/langchain/test.py"
  ePdfContent <- load pdfFilePath
  print $ splitText defaultCharacterSplitterOps "Nice to meet you\n good to be here"
  splitedPdfContent <- loadAndSplit pdfFilePath
  splitedPdfContent2 <- loadAndSplit fileLoaderPath
  print (ePdfContent, splitedPdfContent)
  print splitedPdfContent2
  
  putStrLn "\n=== Embeddings Examples ==="
  -- Creating an embeddings instance using OllamaEmbeddings.
  let ollamaEmbedding = OllamaEmbeddings "nomic-embed-text:latest" Nothing Nothing
  case ePdfContent of
    Left _ -> pure ()
    Right pdfContent -> do 
      embeddingDocRes <- embedDocuments ollamaEmbedding pdfContent
      print embeddingDocRes
  embeddedQuery <- embedQuery ollamaEmbedding "Nice to meet you"
  print embeddedQuery

  putStrLn "\n=== Memory and Chat History Examples ==="
  let wbMem = WindowBufferMemory {
                maxWindowSize = 2,
                windowBufferMessages = chatMsg2 
              }
  eChatRes <- chat ollamaLLM chatMsg (Just $ defaultParams { temperature = Just 0.5 })
  newWbMem <- case eChatRes of
    Left _ -> pure wbMem
    Right r -> do 
      wbMem2 <- addAiMessage wbMem r
      wbMem3 <- addUserMessage (fromRight wbMem wbMem2) "And then what is a functor?"
      wbMem4 <- Chain.invoke (fromRight wbMem wbMem3) "What is Applicative"
      pure $ fromRight wbMem wbMem3
  print newWbMem
  eChatRes2 <- chat ollamaLLM (windowBufferMessages newWbMem) Nothing
  print eChatRes2

  putStrLn "\n=== Output Parser Examples ==="
  let didItParse = parse "That is very true" :: Either String Bool
  print didItParse

  let didItParseAsWell = parse "nice, to, meet, you" :: Either String CommaSeparatedList 
  print didItParseAsWell
 
  let parseJ = parse "{ \"age\" : 2, \"empId\" : \"as\" }" :: Either String (JSONOutputStructure SomeVal)
  print parseJ
  
  let parse4 = parse "1. Vendor onboarding process for business integration 2. Step-by-step guide to vendor registration and setup 3. Best practices for effective vendor due diligence during onboarding" :: Either String NumberSeparatedList
  print parse4

  putStrLn "\n=== In-Memory Vector Store and Retrieval Examples ==="
  -- Create an in-memory vector store using the OllamaEmbeddings.
  let inMem = emptyInMemoryVectorStore ollamaEmbedding 
  eRes <- addDocuments inMem $ 
            fromRight 
                ([Document {pageContent = "Hello World", metadata = Map.fromList []}]) 
                ePdfContent
  let newInMem = fromRight inMem eRes
  eSimilarity <- similaritySearch newInMem "Quality Assurance & Product Specifications" 1
  print eSimilarity

  -- Use the vector store retriever interface.
  let vsRet = VectorStoreRetriever newInMem
  eVsRet <- _get_relevant_documents vsRet "Quality Assurance & Product Specifications"
  print eVsRet

  -- Multi-query retriever using the vector store and OllamaLLM.
  let mqRet = newMultiQueryRetriever vsRet ollamaLLM
  eMulQueryRes <- generateQueries ollamaLLM defaultQueryGenerationPrompt "How to do vendor onboarding" 3 True
  print eMulQueryRes

  putStrLn "\n=== Wikipedia Tool Examples ==="
  -- Run the WikipediaTool to fetch a summary.
  eToolResult <- runTool defaultWikipediaTool "Haskell programming language"
  eToolResult2 <- Chain.invoke defaultWikipediaTool "Nim programming language"
  print eToolResult2

  putStrLn "\n=== Agent (ReAct) Example ==="
  eReactRes <- createReactAgent ollamaLLM []
  case eReactRes of
    Left _ -> pure ()
    Right reactRes -> do
        eRes <- runAgent reactRes (AgentState wbMem [] []) "Write a Caesar cipher in Haskell"
        print eRes

