{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Langchain.Runnable.Chain
  ( RunnableBranch (..)
  , RunnableMap (..)
  , RunnableSequence
  , runBranch
  , runMap
  , runSequence
  , chain
  , branch
  , buildSequence
  , appendSequence 
  , (|>)
  ) where

import Data.List (find)
import Langchain.Runnable.Core

-- | Provides a convenient way to compose runnables
chain ::
  (Runnable r1, Runnable r2, RunnableOutput r1 ~ RunnableInput r2) =>
  r1 ->
  r2 ->
  RunnableInput r1 ->
  IO (Either String (RunnableOutput r2))
chain r1 r2 input = do
  output1 <- invoke r1 input
  case output1 of
    Left err -> return $ Left err
    Right intermediate -> invoke r2 intermediate

-- | Create a new Runnable from two existing ones that can run in parallel
branch ::
  (Runnable r1, Runnable r2, a ~ RunnableInput r1, a ~ RunnableInput r2) =>
  r1 ->
  r2 ->
  a ->
  IO (Either String (RunnableOutput r1, RunnableOutput r2))
branch r1 r2 input = do
  result1 <- invoke r1 input
  result2 <- invoke r2 input
  return $ (,) <$> result1 <*> result2

-- | A branching Runnable that selects which Runnable to invoke based on a condition
data RunnableBranch a b
  = forall r.
    (Runnable r, RunnableInput r ~ a, RunnableOutput r ~ b) =>
    RunnableBranch [(a -> Bool, r)] r -- List of (condition, runnable) pairs and a default runnable

-- | Run a branch by selecting the first Runnable whose condition matches the input
runBranch :: RunnableBranch a b -> a -> IO (Either String b)
runBranch (RunnableBranch options defaultR) input =
  case find (\(cond, _) -> cond input) options of
    Just (_, r) -> invoke r input
    Nothing -> invoke defaultR input

instance Runnable (RunnableBranch a b) where
  type RunnableInput (RunnableBranch a b) = a
  type RunnableOutput (RunnableBranch a b) = b

  invoke = runBranch

-- | A Runnable that applies a function to transform the input
data RunnableMap a b c
  = forall r.
    (Runnable r, RunnableInput r ~ b, RunnableOutput r ~ c) =>
    RunnableMap (a -> b) (c -> c) r -- input transform, output transform, and the runnable

-- | Run with input transformation and output transformation
runMap :: RunnableMap a b c -> a -> IO (Either String c)
runMap (RunnableMap inputFn outputFn r) input = do
  result <- invoke r (inputFn input)
  return $ fmap outputFn result

instance Runnable (RunnableMap a b c) where
  type RunnableInput (RunnableMap a b c) = a
  type RunnableOutput (RunnableMap a b c) = c

  invoke = runMap

data RunnableSequence a b where
  RSNil :: RunnableSequence a a -- the empty chain, where the input and output types are the same.
  RSCons ::
    (Runnable r, RunnableInput r ~ a, RunnableOutput r ~ c) =>
    r ->
    RunnableSequence c b ->
    RunnableSequence a b -- RSCons adds a runnable at the front of the chain.

-- | Run a sequence of runnables, chaining the output of one as input to the next.
runSequence :: RunnableSequence a b -> RunnableInputHead a -> IO (Either String b)
runSequence RSNil input = return (Right input)
runSequence (RSCons r rs) input = do
  result <- invoke r input
  case result of
    Left err -> return (Left err)
    Right out -> runSequence rs out

instance Runnable (RunnableSequence a b) where
  type RunnableInput (RunnableSequence a b) = a
  type RunnableOutput (RunnableSequence a b) = b

  invoke = runSequence

-- | A type synonym to indicate the input type of the first runnable.
type RunnableInputHead a = a

buildSequence ::
  ( Runnable r1
  , Runnable r2
  , RunnableOutput r1 ~ RunnableInput r2
  ) =>
  r1 ->
  r2 ->
  RunnableSequence (RunnableInput r1) (RunnableOutput r2)
buildSequence r1 r2 = RSCons r1 (RSCons r2 RSNil)

appendSequence ::
  ( Runnable r2
  , RunnableOutput (RunnableSequence a b) ~ (RunnableInput r2)
  ) =>
  RunnableSequence a b ->
  r2 ->
  RunnableSequence a (RunnableOutput r2)
appendSequence RSNil r = RSCons r RSNil
appendSequence (RSCons r1 rs) r2 = RSCons r1 (appendSequence rs r2)

-- | Operator version of chain for more readable composition
(|>) ::
  (Runnable r1, Runnable r2, RunnableOutput r1 ~ RunnableInput r2) =>
  r1 ->
  r2 ->
  RunnableInput r1 ->
  IO (Either String (RunnableOutput r2))
(|>) = chain
infix 4 |>
