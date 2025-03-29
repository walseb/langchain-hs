{-# LANGUAGE RecordWildCards #-}
module Langchain.VectorStore.InMemory 
 (
    InMemory(..)
  , fromDocuments 
  , emptyInMemoryVectorStore
  ) where

import Langchain.VectorStore.Core 
import Langchain.Embeddings.Core
import Langchain.DocumentLoader.Core (Document)
import Data.List (sortBy)
import Data.Ord (comparing)
import qualified Data.Map.Strict as Map
import Data.Int (Int64)

-- | Compute the dot product of two vectors.
dotProduct :: [Float] -> [Float] -> Float
dotProduct a b = sum $ zipWith (*) a b

-- | Compute the Euclidean norm of a vector.
norm :: [Float] -> Float
norm a = sqrt $ sum $ map (^(2 :: Int)) a

-- | Compute the cosine similarity between two vectors.
cosineSimilarity :: [Float] -> [Float] -> Float
cosineSimilarity a b = dotProduct a b / (norm a * norm b)

-- | Create an empty in-memory vector store with the given embedding model.
emptyInMemoryVectorStore :: Embeddings m => m -> InMemory m
emptyInMemoryVectorStore model = InMemory model Map.empty

-- | Create an in-memory vector store from a list of documents using the provided embedding model.
fromDocuments :: Embeddings m => m -> [Document] -> IO (Either String (InMemory m))
fromDocuments model docs = do
  let vs = emptyInMemoryVectorStore model
  addDocuments vs docs

data Embeddings m => InMemory m = InMemory { 
    embeddingModel :: m
  , store :: Map.Map Int64 (Document, [Float])
  }
  deriving (Show, Eq)

instance Embeddings m => VectorStore (InMemory m) where
  
  addDocuments inMem docs = do
    eRes <- embedDocuments (embeddingModel inMem) docs
    case eRes of
      Left err -> pure $ Left err
      Right floats -> do 
        let currStore = store inMem
            mbMaxKey = (Map.lookupMax currStore)
            newStore = Map.fromList $ zip [(maybe 1 (\x -> fst x + 1) mbMaxKey)..] (zip docs floats)
            newInMem = inMem { store = Map.union newStore currStore }
        pure $ Right newInMem

  delete inMem ids = do
    let currStore = store inMem
        newStore = foldl (\acc i -> Map.delete i acc) currStore ids
        newInMem = inMem { store = newStore }
    pure $ Right newInMem

  similaritySearch vs query k = do
    eQueryEmbedding <- embedQuery (embeddingModel vs) query
    case eQueryEmbedding of
      Left err -> return $ Left err
      Right queryVec -> similaritySearchByVector vs queryVec k

  similaritySearchByVector vs queryVec k = do
    let similarities = map (\(doc, vec) -> (doc, cosineSimilarity queryVec vec)) (map snd $ Map.toList $ store vs)
        sorted = sortBy (comparing (negate . snd)) similarities  -- Sort in descending order
        topK = take k sorted
    return $ Right $ map fst topK

{-
ghci> let x = OllamaEmbeddings "nomic-embed-text:latest" Nothing Nothing
ghci> let inMem = emptyInMemoryVectorStore x
ghci> eRes <- addDocuments inMem [Document "Hello World" empty, Document "Nice to meet you" empty]
ghci> let newInMem = fromRight inMem eRes
ghci> similaritySearch newInMem "World" 1
Right [Document {pageContent = "Hello World", metadata = fromList []}]
ghci> similaritySearch newInMem "Meet you" 1
Right [Document {pageContent = "Nice to meet you", metadata = fromList []}]
-}
