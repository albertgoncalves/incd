{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative (liftA2)
import Data.Either (rights)
import Data.Text (Text, splitOn)
import Data.Text.IO (readFile)
import Data.Text.Read (decimal, signed)
import System.Environment (getArgs)
import Test (Loc (..), test)
import Vm (Vm (..), run)
import Prelude hiding (readFile)

#define TEST test (Loc (__FILE__, __LINE__))

addNewline :: IO () -> IO ()
addNewline f = do
  f
  putChar '\n'

tests :: IO ()
tests = do
  TEST (f [1, 0, 0, 0, 99]) [2, 0, 0, 0, 99]
  TEST (f [2, 3, 0, 3, 99]) [2, 3, 0, 6, 99]
  TEST (f [2, 4, 4, 5, 99, 0]) [2, 4, 4, 5, 99, 9801]
  TEST (f [1, 1, 1, 4, 99, 5, 6, 0, 99]) [30, 1, 1, 4, 2, 5, 6, 0, 99]
  TEST (f [1002, 4, 3, 4, 33]) [1002, 4, 3, 4, 99]
  TEST (f [1101, 100, -1, 4, 0]) [1101, 100, -1, 4, 99]
  TEST (head $ getOutput $ run $ Vm [3, 0, 4, 0, 99] [] [1234] []) 1234
  where
    f = getProgram . run . (\xs -> Vm xs [] [] [])

intoProgram :: Text -> [Int]
intoProgram = rights . map (fmap fst . signed decimal) . splitOn ","

-- NOTE: See `https://adventofcode.com/2019/day/2`.
solve2 :: Text -> IO ()
solve2 x = do
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
  where
    xs = intoProgram x

-- NOTE: See `https://adventofcode.com/2019/day/5`.
solve5 :: Text -> IO ()
solve5 x = do
  TEST (getOutput $ run $ Vm xs [] [1] []) [5821753, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  where
    xs = intoProgram x

main :: IO ()
main = do
  addNewline tests
  [path2, path5] <- getArgs
  mapM_
    (\(f, x) -> addNewline . f =<< readFile x)
    [ (solve2, path2),
      (solve5, path5)
    ]
