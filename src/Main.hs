{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative (liftA2)
import Control.Exception (assert)
import Data.Either (lefts, rights)
import Data.Text (Text, splitOn)
import Data.Text.IO (readFile)
import Data.Text.Read (decimal, signed)
import System.Environment (getArgs)
import Test (Loc (..), test)
import Vm (Vm (..), run)
import Prelude hiding (readFile)

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
  where
    f0 = getProgram . run . (\xs -> Vm xs [] [] [])
    f1 = head . getOutput . run . (\(xs, input) -> Vm xs [] input [])

-- NOTE: See `https://adventofcode.com/2019/day/2`.
solve2 :: [Int] -> IO ()
solve2 xs = do
  TEST (head $ getProgram $ run (Vm xs [(1, 12), (2, 2)] [] [])) 9706670
  TEST
    ( fst $
        head $
          dropWhile ((/= 19690720) . snd) $
            map
              ( \(p0, p1) ->
                  ( (100 * p0) + p1,
                    head $ getProgram $ run (Vm xs [(1, p0), (2, p1)] [] [])
                  )
              )
              (liftA2 (,) [0 .. 99] [0 .. 99])
    )
    2552

-- NOTE: See `https://adventofcode.com/2019/day/5`.
solve5 :: [Int] -> IO ()
solve5 xs = do
  TEST (getOutput $ run $ Vm xs [] [1] []) [5821753, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  TEST (getOutput $ run $ Vm xs [] [5] []) [11956381]

intoProgram :: Text -> [Int]
intoProgram x = assert (null $ lefts xs) $ rights xs
  where
    xs :: [Either String Int]
    xs = map (fmap fst . signed decimal) $ splitOn "," x

main :: IO ()
main = do
  addNewline tests
  [path2, path5] <- getArgs
  mapM_
    (\(f, x) -> addNewline . f . intoProgram =<< readFile x)
    [ (solve2, path2),
      (solve5, path5)
    ]
  where
    addNewline :: IO () -> IO ()
    addNewline f = do
      f
      putChar '\n'
