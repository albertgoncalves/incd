{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative (liftA2)
import Data.Either (rights)
import Data.IntMap.Lazy (IntMap, elems, fromList)
import Data.List (permutations)
import Data.Text (splitOn)
import Data.Text.IO (readFile)
import Data.Text.Read (decimal, signed)
import System.Environment (getArgs)
import Test (Loc (..), test)
import Vm (Vm (..), VmState (..), newState, patch, run, withInput)
import Prelude hiding (readFile)

intoMap :: [Int] -> IntMap Int
intoMap = fromList . zip [0 ..]

chain0 :: [Int] -> [Int] -> Int
chain0 program [in0, in1, in2, in3, in4] = head out4
  where
    f = getOutput . getState . run . Vm (intoMap program) . withInput newState
    out0 = f [in0, 0]
    out1 = f $ in1 : out0
    out2 = f $ in2 : out1
    out3 = f $ in3 : out2
    out4 = f $ in4 : out3
chain0 _ _ = undefined

-- NOTE: See `https://cuddly-octo-palm-tree.com/posts/2021-03-07-review-whyfp/`.
chain1 :: [Int] -> [Int] -> Int
chain1 program [in0, in1, in2, in3, in4] = last out4
  where
    f =
      reverse
        . getOutput
        . getState
        . run
        . Vm (intoMap program)
        . withInput newState
    out0 = f $ in0 : 0 : out4
    out1 = f $ in1 : out0
    out2 = f $ in2 : out1
    out3 = f $ in3 : out2
    out4 = f $ in4 : out3
chain1 _ _ = undefined

#define TEST test (Loc (__FILE__, __LINE__))

tests :: IO ()
tests = do
  TEST (f0 [1, 0, 0, 0, 99]) [2, 0, 0, 0, 99]
  TEST (f0 [2, 3, 0, 3, 99]) [2, 3, 0, 6, 99]
  TEST (f0 [2, 4, 4, 5, 99, 0]) [2, 4, 4, 5, 99, 9801]
  TEST (f0 [1, 1, 1, 4, 99, 5, 6, 0, 99]) [30, 1, 1, 4, 2, 5, 6, 0, 99]
  TEST (f0 [1002, 4, 3, 4, 33]) [1002, 4, 3, 4, 99]
  TEST (f0 [1101, 100, -1, 4, 0]) [1101, 100, -1, 4, 99]
  TEST (f1 [3, 0, 4, 0, 99] [1234]) [1234]
  TEST (f1 [3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8] [8]) [1]
  TEST (f1 [3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8] [7]) [0]
  TEST (f1 [3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8] [7]) [1]
  TEST (f1 [3, 3, 1108, -1, 8, 3, 4, 3, 99] [8]) [1]
  TEST (f1 [3, 3, 1108, -1, 8, 3, 4, 3, 99] [9]) [0]
  TEST (f1 [3, 3, 1107, -1, 8, 3, 4, 3, 99] [7]) [1]
  TEST (f1 [3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9] [-9]) [1]
  TEST (f1 [3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9] [0]) [0]
  TEST (f1 [3, 3, 1105, -1, 9, 1101, 0, 0, 12, 4, 12, 99, 1] [-1]) [1]
  TEST
    ( chain0
        [3, 15, 3, 16, 1002, 16, 10, 16, 1, 16, 15, 15, 4, 15, 99, 0, 0]
        [4, 3, 2, 1, 0]
    )
    43210
  TEST
    ( chain0
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
    ( chain0
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
  TEST
    ( chain1
        [ 3,
          26,
          1001,
          26,
          -4,
          26,
          3,
          27,
          1002,
          27,
          2,
          27,
          1,
          27,
          26,
          27,
          4,
          27,
          1001,
          28,
          -1,
          28,
          1005,
          28,
          6,
          99,
          0,
          0,
          5
        ]
        [9, 8, 7, 6, 5]
    )
    139629729
  TEST
    ( chain1
        [ 3,
          52,
          1001,
          52,
          -5,
          52,
          3,
          53,
          1,
          52,
          56,
          54,
          1007,
          54,
          5,
          55,
          1005,
          55,
          26,
          1001,
          54,
          -5,
          54,
          1105,
          1,
          12,
          1,
          53,
          54,
          53,
          1008,
          54,
          0,
          55,
          1001,
          55,
          1,
          55,
          2,
          53,
          55,
          53,
          4,
          53,
          1001,
          56,
          -1,
          56,
          1005,
          56,
          6,
          99,
          0,
          0,
          0,
          0,
          10
        ]
        [9, 7, 8, 5, 6]
    )
    18216
  TEST
    ( take 16 $
        f0
          [ 109,
            1,
            204,
            -1,
            1001,
            100,
            1,
            100,
            1008,
            100,
            16,
            101,
            1006,
            101,
            0,
            99
          ]
    )
    [109, 1, 204, -1, 1001, 100, 1, 100, 1008, 100, 16, 101, 1006, 101, 0, 99]
  TEST (f1 [1102, 34915192, 34915192, 7, 4, 7, 99, 0] []) [1219070632396864]
  TEST (f1 [104, 1125899906842624, 99] []) [1125899906842624]
  where
    f0 :: [Int] -> [Int]
    f0 program = elems $ getProgram $ run $ Vm (intoMap program) newState
    f1 program input =
      getOutput $
        getState $
          run $ Vm (intoMap program) $ VmState True 0 0 input []

-- NOTE: See `https://adventofcode.com/2019/day/2`.
solve2 :: [Int] -> IO ()
solve2 program = do
  TEST (f [(1, 12), (2, 2)]) 9706670
  TEST
    ( fst $
        head $
          dropWhile ((/= 19690720) . snd) $
            map
              (\(p0, p1) -> ((100 * p0) + p1, f [(1, p0), (2, p1)]))
              (let xs = [0 .. 99] :: [Int] in liftA2 (,) xs xs)
    )
    2552
  where
    f = head . elems . getProgram . run . (`Vm` newState) . patch program'
    program' = intoMap program

-- NOTE: See `https://adventofcode.com/2019/day/5`.
solve5 :: [Int] -> IO ()
solve5 program = do
  TEST (f 1) [5821753, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  TEST (f 5) [11956381]
  where
    f in0 = getOutput $ getState $ run $ Vm program' $ withInput newState [in0]
    program' = intoMap program

-- NOTE: See `https://adventofcode.com/2019/day/7`.
solve7 :: [Int] -> IO ()
solve7 program = do
  TEST (f chain0 [0 .. 4]) 199988
  TEST (f chain1 [5 .. 9]) 17519904
  where
    f c = maximum . map (c program) . permutations

-- NOTE: See `https://adventofcode.com/2019/day/9`.
solve9 :: [Int] -> IO ()
solve9 program = do
  TEST (f [1]) [4234906522]
  TEST (f [2]) [60962]
  where
    f = getOutput . getState . run . Vm program' . withInput newState
    program' = intoMap program

main :: IO ()
main = do
  addNewline tests
  [path2, path5, path7, path9] <- getArgs
  mapM_
    ( \(f, x) ->
        addNewline
          . f
          . rights
          . map (fmap fst . signed decimal)
          . splitOn ","
          =<< readFile x
    )
    [ (solve2, path2),
      (solve5, path5),
      (solve7, path7),
      (solve9, path9)
    ]
  where
    addNewline :: IO () -> IO ()
    addNewline f = do
      f
      putChar '\n'
