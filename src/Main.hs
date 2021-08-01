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
import Vm (Vm (..), VmState (..), appendInput, newState, patch, run)
import Prelude hiding (readFile)

chain0 :: [Int] -> [Int] -> Int
chain0 program = foldl' f 0
  where
    f in1 in0 =
      head $
        getOutput $
          getState $
            run (length program) $
              Vm program $
                foldl' appendInput newState [in0, in1]

chain1 :: [Int] -> [Int] -> Int
chain1 program inputs = loop (map (Vm program . appendInput newState) inputs) 0
  where
    loop :: [Vm] -> Int -> Int
    loop ((Vm program' state) : vms) in1 =
      case run (length program) $ Vm program' $ appendInput state in1 of
        (Vm _ (VmState False _ _ _ [out0])) ->
          case vms of
            [] -> out0
            vms' -> loop vms' out0
        (Vm program'' (VmState True index base input [out0])) ->
          loop (vms ++ [Vm program'' $ VmState True index base input []]) out0
        _ -> undefined
    loop _ _ = undefined

#define TEST test (Loc (__FILE__, __LINE__))

tests :: IO ()
tests = do
  TEST (f0 5 [1, 0, 0, 0, 99]) [2, 0, 0, 0, 99]
  TEST (f0 5 [2, 3, 0, 3, 99]) [2, 3, 0, 6, 99]
  TEST (f0 6 [2, 4, 4, 5, 99, 0]) [2, 4, 4, 5, 99, 9801]
  TEST (f0 9 [1, 1, 1, 4, 99, 5, 6, 0, 99]) [30, 1, 1, 4, 2, 5, 6, 0, 99]
  TEST (f0 5 [1002, 4, 3, 4, 33]) [1002, 4, 3, 4, 99]
  TEST (f0 5 [1101, 100, -1, 4, 0]) [1101, 100, -1, 4, 99]
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
          1008
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
    f0 cap program = getProgram $ run cap $ Vm program newState
    f1 program input =
      getOutput $
        getState $
          run (length program) $ Vm program $ VmState True 0 0 input []

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
    f =
      head
        . getProgram
        . run (length program)
        . (`Vm` newState)
        . patch program

-- NOTE: See `https://adventofcode.com/2019/day/5`.
solve5 :: [Int] -> IO ()
solve5 program = do
  TEST (f 1) [5821753, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  TEST (f 5) [11956381]
  where
    f in0 =
      getOutput $
        getState $
          run (length program) $ Vm program $ appendInput newState in0

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
  TEST (f 1) [4234906522]
  TEST (f 2) [60962]
  where
    f = getOutput . getState . run 1077 . Vm program . appendInput newState

intoProgram :: Text -> [Int]
intoProgram x = assert (null $ lefts xs) $ rights xs
  where
    xs :: [Either String Int]
    xs = map (fmap fst . signed decimal) $ splitOn "," x

main :: IO ()
main = do
  addNewline tests
  [path2, path5, path7, path9] <- getArgs
  mapM_
    (\(f, x) -> addNewline . f . intoProgram =<< readFile x)
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
