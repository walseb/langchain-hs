{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Langchain.Retriever.MultiQueryRetriever
  ( MultiQueryRetriever (..)
  , newMultiQueryRetriever
  , QueryGenerationPrompt (..)
  , defaultQueryGenerationPrompt
  , newMultiQueryRetrieverWithConfig 
  ) where

import Langchain.DocumentLoader.Core (Document)
import Langchain.LLM.Core (LLM (..))
import Langchain.OutputParser.Core (CommaSeparatedList (..), OutputParser (..))
import Langchain.PromptTemplate (PromptTemplate (..), renderPrompt)
import Langchain.Retriever.Core (Retriever (..))
import qualified Langchain.Runnable.Core as Run

import Data.Either (rights)
import Data.List (nub)
import qualified Data.Map.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T

-- | A prompt template for generating multiple queries from a single input query
newtype QueryGenerationPrompt = QueryGenerationPrompt PromptTemplate
  deriving (Show, Eq)

-- | Default prompt for generating multiple queries
defaultQueryGenerationPrompt :: QueryGenerationPrompt
defaultQueryGenerationPrompt =
  QueryGenerationPrompt $
    PromptTemplate
      { templateString =
          T.unlines
            [ "You are an AI language model assistant that helps users by generating multiple search queries based on their initial query."
            , "These queries should help retrieve relevant documents or information from a vector database."
            , ""
            , "Original query: {query}"
            , ""
            , "Please generate {num_queries} different versions of this query that will help the user find the most relevant information."
            , "The queries should be different but related to the original query."
            , "Return these queries as a comma-separated list."
            ]
      }

-- | Configuration for MultiQueryRetriever
data MultiQueryRetrieverConfig = MultiQueryRetrieverConfig
  { numQueries :: Int
  -- ^ Number of queries to generate
  , queryGenerationPrompt :: QueryGenerationPrompt
  -- ^ Prompt template for query generation
  , includeMergeDocs :: Bool
  -- ^ Whether to include merged documents
  , includeOriginalQuery :: Bool
  -- ^ Whether to include results from original query
  }

-- | Default configuration for MultiQueryRetriever
defaultMultiQueryRetrieverConfig :: MultiQueryRetrieverConfig
defaultMultiQueryRetrieverConfig =
  MultiQueryRetrieverConfig
    { numQueries = 3
    , queryGenerationPrompt = defaultQueryGenerationPrompt
    , includeMergeDocs = True
    , includeOriginalQuery = True
    }

{- | MultiQueryRetriever generates multiple queries from a single input query,
retrieves documents for each generated query, and combines the results.
-}
data MultiQueryRetriever a m = MultiQueryRetriever
  { retriever :: a
  -- ^ The base retriever
  , llm :: m
  -- ^ The language model for generating queries
  , config :: MultiQueryRetrieverConfig
  -- ^ Configuration
  }

-- | Create a new MultiQueryRetriever with default configuration
newMultiQueryRetriever :: a -> m -> MultiQueryRetriever a m
newMultiQueryRetriever r l =
  MultiQueryRetriever
    { retriever = r
    , llm = l
    , config = defaultMultiQueryRetrieverConfig
    }

-- | Create a new MultiQueryRetriever with custom configuration
newMultiQueryRetrieverWithConfig ::
  a -> m -> MultiQueryRetrieverConfig -> MultiQueryRetriever a m
newMultiQueryRetrieverWithConfig r l c =
  MultiQueryRetriever
    { retriever = r
    , llm = l
    , config = c
    }

-- | Generate multiple queries using the LLM
generateQueries ::
  LLM m => m -> QueryGenerationPrompt -> Text -> Int -> Bool -> IO (Either String [Text])
generateQueries model (QueryGenerationPrompt promptTemplate) query n includeOriginal = do
  let vars = HM.fromList [("query", query), ("num_queries", T.pack $ show n)]
  case renderPrompt promptTemplate vars of
    Left err -> return $ Left err
    Right prompt -> do
      result <- generate model prompt Nothing
      case result of
        Left err -> return $ Left err
        Right response -> do
          case parse response :: Either String CommaSeparatedList of
            Left err -> return $ Left $ "Failed to parse LLM response: " ++ err
            Right (CommaSeparatedList queries) -> do
              let uniqueQueries = nub $ filter (not . T.null) queries
              return $
                Right $
                  if includeOriginal
                    then query : uniqueQueries
                    else uniqueQueries

-- | Combine documents from multiple queries, removing duplicates
combineDocuments :: [[Document]] -> [Document]
combineDocuments docLists =
  -- This is a simplified approach. In a production system, you'd want a more
  -- sophisticated way to identify and rank duplicate documents
  nub $ concat docLists

-- | Implement Retriever instance for MultiQueryRetriever
instance (Retriever a, LLM m) => Retriever (MultiQueryRetriever a m) where
  _get_relevant_documents r query = do
    let baseRetriever = retriever r
        model = llm r
        cfg = config r

    -- Generate multiple queries
    queriesResult <-
      generateQueries
        model
        (queryGenerationPrompt cfg)
        query
        (numQueries cfg)
        (includeOriginalQuery cfg)

    case queriesResult of
      Left err -> return $ Left $ "Error generating queries: " ++ err
      Right queries -> do
        -- Get documents for each query
        results <- mapM (_get_relevant_documents baseRetriever) queries

        -- Filter successful results
        let validResults = rights results

        if null validResults
          then return $ Left "No valid results from any query"
          else return $ Right $ combineDocuments validResults

{-
 ghci> :set -XOverloadedStrings
 ghci> let ollamaEmbed = OllamaEmbeddings "nomic-embed-text:latest" Nothing Nothing
 ghci> let vs = emptyInMemoryVectorStore ollamaEmbed
 ghci> import Data.Map (empty)
 ghci> import Data.Either
 ghci> newVs <- addDocuments vs [Document "Tushar is 25 years old." empty]
 ghci> let newVs_ = fromRight vs newVs
 ghci> let vRet = VectorStoreRetriever newVs_
 ghci> let ollamLLM = Ollama "llama3.2" []
 ghci> let mqRet = newMultiQueryRetriever vRet ollamLLM
 ghci> documents <- _get_relevant_documents mqRet "How old is Tushar?"
 ghci> documents
    Right [Document {pageContent = "Tushar is 25 years old.", metadata = fromList []}]
 -}

instance (Retriever a, LLM m) => Run.Runnable (MultiQueryRetriever a m) where
  type RunnableInput (MultiQueryRetriever a m) = Text
  type RunnableOutput (MultiQueryRetriever a m) = [Document]
  
  invoke r query = _get_relevant_documents r query
