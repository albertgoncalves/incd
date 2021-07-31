{-# LANGUAGE FlexibleContexts #-}

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

data Vm = Vm
  { getProgram :: [Int],
    getPatches :: [(Int, Int)],
    getInput :: [Int],
    getOutput :: [Int]
  }

readVal :: (MArray a Int m) => a Int Int -> Int -> Int -> m Int
readVal array 0 = readArray array <=< readArray array
readVal array 1 = readArray array
readVal _ _ = undefined

readDst :: (MArray a Int m) => a Int Int -> Int -> Int -> m Int
readDst array 0 = readArray array
readDst _ _ = undefined

loop :: (MArray a Int m) => a Int Int -> [Int] -> [Int] -> Int -> m [Int]
loop array input output i = do
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
      loop array input output (i + 4)
    2 -> do
      val0 <- readVal array m0 (i + 1)
      val1 <- readVal array m1 (i + 2)
      dst <- readDst array m2 (i + 3)
      writeArray array dst (val0 * val1)
      loop array input output (i + 4)
    3 -> case input of
      (val : input') -> do
        dst <- readDst array m0 (i + 1)
        writeArray array dst val
        loop array input' output (i + 2)
      _ -> undefined
    4 -> do
      val <- readVal array m0 (i + 1)
      loop array input (val : output) (i + 2)
    5 -> do
      val0 <- readVal array m0 (i + 1)
      val1 <- readVal array m1 (i + 2)
      if val0 /= 0
        then loop array input output val1
        else loop array input output (i + 3)
    6 -> do
      val0 <- readVal array m0 (i + 1)
      val1 <- readVal array m1 (i + 2)
      if val0 == 0
        then loop array input output val1
        else loop array input output (i + 3)
    7 -> do
      val0 <- readVal array m0 (i + 1)
      val1 <- readVal array m1 (i + 2)
      dst <- readDst array m2 (i + 3)
      writeArray array dst $
        if val0 < val1
          then 1
          else 0
      loop array input output (i + 4)
    8 -> do
      val0 <- readVal array m0 (i + 1)
      val1 <- readVal array m1 (i + 2)
      dst <- readDst array m2 (i + 3)
      writeArray array dst $
        if val0 == val1
          then 1
          else 0
      loop array input output (i + 4)
    99 -> return output
    _ -> undefined

-- NOTE: See `https://hackage.haskell.org/package/array-0.5.4.0/docs/src/Data.Array.ST.html#runSTUArray`
run :: Vm -> Vm
run (Vm program patches input output) = Vm (elems program') [] [] output''
  where
    (program', output'') =
      runST $ do
        array <- newListArray (0, length program - 1) program
        mapM_ (uncurry $ writeArray array) patches
        output' <- loop array input output 0
        array' <- unsafeFreezeSTUArray array
        return (array', output')
