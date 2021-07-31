{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Strict #-}

module Vm where

import Control.Monad ((<=<))
import Control.Monad.ST (runST)
import Data.Array.Base (unsafeFreezeSTUArray)
import Data.Array.IArray (elems)
import Data.Array.MArray
  ( MArray,
    newListArray,
    readArray,
    writeArray,
  )

data VmState = VmState
  { getAlive :: Bool,
    getIndex :: Int,
    getInput :: [Int],
    getOutput :: [Int]
  }

data Vm = Vm
  { getProgram :: [Int],
    getPatches :: [(Int, Int)],
    getState :: VmState
  }

newState :: VmState
newState = VmState True 0 [] []

appendInput :: VmState -> Int -> VmState
appendInput (VmState alive index input output) in1 =
  VmState alive index (input ++ [in1]) output

readVal :: (MArray a Int m) => a Int Int -> Int -> Int -> m Int
readVal array 0 = readArray array <=< readArray array
readVal array 1 = readArray array
readVal _ _ = undefined

readDst :: (MArray a Int m) => a Int Int -> Int -> Int -> m Int
readDst array 0 = readArray array
readDst _ _ = undefined

loop :: (MArray a Int m) => a Int Int -> VmState -> m VmState
loop array (VmState _ i input output) = do
  x <- readArray array i
  let m0 = (x `div` 100) `mod` 10
      m1 = (x `div` 1000) `mod` 10
      m2 = (x `div` 10000) `mod` 10
  case x `mod` 100 of
    1 -> do
      val0 <- readVal array m0 (i + 1)
      val1 <- readVal array m1 (i + 2)
      dst <- readDst array m2 (i + 3)
      writeArray array dst (val0 + val1)
      loop array (VmState True (i + 4) input output)
    2 -> do
      val0 <- readVal array m0 (i + 1)
      val1 <- readVal array m1 (i + 2)
      dst <- readDst array m2 (i + 3)
      writeArray array dst (val0 * val1)
      loop array (VmState True (i + 4) input output)
    3 -> case input of
      (val : input') -> do
        dst <- readDst array m0 (i + 1)
        writeArray array dst val
        loop array (VmState True (i + 2) input' output)
      [] -> return $ VmState True i [] output
    4 -> do
      val <- readVal array m0 (i + 1)
      loop array (VmState True (i + 2) input (val : output))
    5 -> do
      val0 <- readVal array m0 (i + 1)
      val1 <- readVal array m1 (i + 2)
      if val0 /= 0
        then loop array (VmState True val1 input output)
        else loop array (VmState True (i + 3) input output)
    6 -> do
      val0 <- readVal array m0 (i + 1)
      val1 <- readVal array m1 (i + 2)
      if val0 == 0
        then loop array (VmState True val1 input output)
        else loop array (VmState True (i + 3) input output)
    7 -> do
      val0 <- readVal array m0 (i + 1)
      val1 <- readVal array m1 (i + 2)
      dst <- readDst array m2 (i + 3)
      writeArray array dst $
        if val0 < val1
          then 1
          else 0
      loop array (VmState True (i + 4) input output)
    8 -> do
      val0 <- readVal array m0 (i + 1)
      val1 <- readVal array m1 (i + 2)
      dst <- readDst array m2 (i + 3)
      writeArray array dst $
        if val0 == val1
          then 1
          else 0
      loop array (VmState True (i + 4) input output)
    99 -> return $ VmState False i input output
    _ -> undefined

-- NOTE: See `https://hackage.haskell.org/package/array-0.5.4.0/docs/src/Data.Array.ST.html#runSTUArray`
run :: Vm -> Vm
run vm@(Vm program patches state@(VmState alive _ _ _)) =
  if alive
    then runST $ do
      array <- newListArray (0, length program - 1) program
      mapM_ (uncurry $ writeArray array) patches
      state' <- loop array state
      program' <- elems <$> unsafeFreezeSTUArray array
      return $ Vm program' [] state'
    else vm
