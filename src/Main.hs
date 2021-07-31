{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative (liftA2)
import Control.Exception (assert)
import Data.Either (lefts, rights)
import Data.List (foldl', permutations)
import Data.Text (Text, splitOn)
import Data.Text.IO (readFile)
import Data.Text.Read (decimal, signed)
import System.Environment (getArgs)
import Test (Loc (..), test)
import Vm (Vm (..), VmState (..), newState, run)
import Prelude hiding (readFile)

chain :: [Int] -> [Int] -> Int
chain xs = foldl' f 0
  where
    f in1 in0 =
      head $
        getOutput $ getState $ run $ Vm xs [] (VmState True 0 [in0, in1] [])

#define TEST test (Loc (__FILE__, __LINE__))

tests :: IO ()
tests = do
  TEST (f0 [1, 0, 0, 0, 99]) [2, 0, 0, 0, 99]
  TEST (f0 [2, 3, 0, 3, 99]) [2, 3, 0, 6, 99]
  TEST (f0 [2, 4, 4, 5, 99, 0]) [2, 4, 4, 5, 99, 9801]
  TEST (f0 [1, 1, 1, 4, 99, 5, 6, 0, 99]) [30, 1, 1, 4, 2, 5, 6, 0, 99]
  TEST (f0 [1002, 4, 3, 4, 33]) [1002, 4, 3, 4, 99]
  TEST (f0 [1101, 100, -1, 4, 0]) [1101, 100, -1, 4, 99]
  TEST (f1 ([3, 0, 4, 0, 99], [1234])) 1234
  TEST (f1 ([3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8], [8])) 1
  TEST (f1 ([3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8], [7])) 0
  TEST (f1 ([3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8], [7])) 1
  TEST (f1 ([3, 3, 1108, -1, 8, 3, 4, 3, 99], [8])) 1
  TEST (f1 ([3, 3, 1108, -1, 8, 3, 4, 3, 99], [9])) 0
  TEST (f1 ([3, 3, 1107, -1, 8, 3, 4, 3, 99], [7])) 1
  TEST (f1 ([3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9], [-9])) 1
  TEST (f1 ([3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9], [0])) 0
  TEST (f1 ([3, 3, 1105, -1, 9, 1101, 0, 0, 12, 4, 12, 99, 1], [-1])) 1
  TEST
    ( chain
        [3, 15, 3, 16, 1002, 16, 10, 16, 1, 16, 15, 15, 4, 15, 99, 0, 0]
        [4, 3, 2, 1, 0]
    )
    43210
  TEST
    ( chain
        [ 3,
          23,
          3,
          24,
          1002,
          24,
          10,
          24,
          1002,
          23,
          -1,
          23,
          101,
          5,
          23,
          23,
          1,
          24,
          23,
          23,
          4,
          23,
          99,
          0,
          0
        ]
        [0, 1, 2, 3, 4]
    )
    54321
  TEST
    ( chain
        [ 3,
          31,
          3,
          32,
          1002,
          32,
          10,
          32,
          1001,
          31,
          -2,
          31,
          1007,
          31,
          0,
          33,
          1002,
          33,
          7,
          33,
          1,
          33,
          31,
          31,
          1,
          32,
          31,
          31,
          4,
          31,
          99,
          0,
          0,
          0
        ]
        [1, 0, 4, 3, 2]
    )
    65210
  where
    f0 = getProgram . run . (\xs -> Vm xs [] newState)
    f1 =
      head
        . getOutput
        . getState
        . run
        . (\(xs, input) -> Vm xs [] (VmState True 0 input []))

-- NOTE: See `https://adventofcode.com/2019/day/2`.
solve2 :: [Int] -> IO ()
solve2 xs = do
  TEST (head $ getProgram $ run (Vm xs [(1, 12), (2, 2)] newState)) 9706670
  TEST
    ( fst $
        head $
          dropWhile ((/= 19690720) . snd) $
            map
              ( \(p0, p1) ->
                  ( (100 * p0) + p1,
                    head $ getProgram $ run (Vm xs [(1, p0), (2, p1)] newState)
                  )
              )
              (liftA2 (,) [0 .. 99] [0 .. 99])
    )
    2552

-- NOTE: See `https://adventofcode.com/2019/day/5`.
solve5 :: [Int] -> IO ()
solve5 xs = do
  TEST
    (f $ Vm xs [] (VmState True 0 [1] []))
    [5821753, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  TEST (f $ Vm xs [] (VmState True 0 [5] [])) [11956381]
  where
    f = getOutput . getState . run

-- NOTE: See `https://adventofcode.com/2019/day/7`.
solve7 :: [Int] -> IO ()
solve7 xs = do
  TEST (maximum $ map (chain xs) $ permutations [0 .. 4]) 199988

intoProgram :: Text -> [Int]
intoProgram x = assert (null $ lefts xs) $ rights xs
  where
    xs :: [Either String Int]
    xs = map (fmap fst . signed decimal) $ splitOn "," x

main :: IO ()
main = do
  addNewline tests
  [path2, path5, path7] <- getArgs
  mapM_
    (\(f, x) -> addNewline . f . intoProgram =<< readFile x)
    [ (solve2, path2),
      (solve5, path5),
      (solve7, path7)
    ]
  where
    addNewline :: IO () -> IO ()
    addNewline f = do
      f
      putChar '\n'
