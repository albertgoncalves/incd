{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Strict #-}

module Vm where

import Control.Monad ((<=<))
import Control.Monad.ST (runST)
import Data.Array.Base (unsafeFreezeSTUArray)
import Data.Array.IArray (elems)
import Data.Array.MArray (MArray, newListArray, readArray, writeArray)

data VmState = VmState
  { getAlive :: Bool,
    getIndex :: Int,
    getBase :: Int,
    getInput :: [Int],
    getOutput :: [Int]
  }

data Vm = Vm
  { getProgram :: [Int],
    getState :: VmState
  }

newState :: VmState
newState = VmState True 0 0 [] []

appendInput :: VmState -> Int -> VmState
appendInput (VmState alive index base input output) in1 =
  VmState alive index base (input ++ [in1]) output

readVal :: (MArray a Int m) => a Int Int -> Int -> Int -> Int -> m Int
readVal array 0 _ = readArray array <=< readArray array
readVal array 1 _ = readArray array
readVal array 2 base = readArray array . (base +) <=< readArray array
readVal _ _ _ = undefined

readDst :: (MArray a Int m) => a Int Int -> Int -> Int -> Int -> m Int
readDst array 0 _ = readArray array
readDst array 2 base = fmap (base +) . readArray array
readDst _ _ _ = undefined

loop :: (MArray a Int m) => a Int Int -> VmState -> m VmState
loop array (VmState _ i base input output) = do
  x <- readArray array i
  let m0 = (x `div` 100) `mod` 10
      m1 = (x `div` 1000) `mod` 10
      m2 = (x `div` 10000) `mod` 10
  case x `mod` 100 of
    1 -> do
      val0 <- readVal array m0 base (i + 1)
      val1 <- readVal array m1 base (i + 2)
      dst <- readDst array m2 base (i + 3)
      writeArray array dst (val0 + val1)
      loop array $ VmState True (i + 4) base input output
    2 -> do
      val0 <- readVal array m0 base (i + 1)
      val1 <- readVal array m1 base (i + 2)
      dst <- readDst array m2 base (i + 3)
      writeArray array dst (val0 * val1)
      loop array $ VmState True (i + 4) base input output
    3 -> case input of
      (val : input') -> do
        dst <- readDst array m0 base (i + 1)
        writeArray array dst val
        loop array $ VmState True (i + 2) base input' output
      [] -> return $ VmState True i base [] output
    4 -> do
      val <- readVal array m0 base (i + 1)
      loop array $ VmState True (i + 2) base input (val : output)
    5 -> do
      val0 <- readVal array m0 base (i + 1)
      val1 <- readVal array m1 base (i + 2)
      if val0 /= 0
        then loop array $ VmState True val1 base input output
        else loop array $ VmState True (i + 3) base input output
    6 -> do
      val0 <- readVal array m0 base (i + 1)
      val1 <- readVal array m1 base (i + 2)
      if val0 == 0
        then loop array $ VmState True val1 base input output
        else loop array $ VmState True (i + 3) base input output
    7 -> do
      val0 <- readVal array m0 base (i + 1)
      val1 <- readVal array m1 base (i + 2)
      dst <- readDst array m2 base (i + 3)
      writeArray array dst $
        if val0 < val1
          then 1
          else 0
      loop array $ VmState True (i + 4) base input output
    8 -> do
      val0 <- readVal array m0 base (i + 1)
      val1 <- readVal array m1 base (i + 2)
      dst <- readDst array m2 base (i + 3)
      writeArray array dst $
        if val0 == val1
          then 1
          else 0
      loop array $ VmState True (i + 4) base input output
    9 -> do
      val <- readVal array m0 base (i + 1)
      loop array $ VmState True (i + 2) (base + val) input output
    99 -> return $ VmState False i base input output
    _ -> undefined

toArray :: (MArray a Int m) => Int -> [Int] -> m (a Int Int)
toArray cap = newListArray (0, cap)

patch :: [Int] -> [(Int, Int)] -> [Int]
patch program patches = runST $ do
  array <- toArray (length program - 1) program
  mapM_ (uncurry $ writeArray array) patches
  elems <$> unsafeFreezeSTUArray array

-- NOTE: See `https://hackage.haskell.org/package/array-0.5.4.0/docs/src/Data.Array.ST.html#runSTUArray`.
run :: Int -> Vm -> Vm
run cap vm@(Vm program state@(VmState alive _ _ _ _)) =
  if alive
    then runST $ do
      array <- toArray cap program
      state' <- loop array state
      (`Vm` state') . elems <$> unsafeFreezeSTUArray array
    else vm
