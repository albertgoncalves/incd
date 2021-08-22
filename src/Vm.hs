module Vm where

import Data.IntMap.Lazy (IntMap, findWithDefault, fromList, insert, union)

data VmState = VmState
  { getAlive :: !Bool,
    getIndex :: !Int,
    getBase :: !Int,
    getInput :: [Int],
    getOutput :: [Int]
  }

data Vm = Vm
  { getProgram :: IntMap Int,
    getState :: !VmState
  }

newState :: VmState
newState = VmState True 0 0 [] []

withInput :: VmState -> [Int] -> VmState
withInput (VmState alive index base in0 output) in1 =
  VmState alive index base (in0 ++ in1) output

(!) :: IntMap Int -> Int -> Int
(!) m k = findWithDefault 0 k m

readVal :: IntMap Int -> Int -> Int -> Int -> Int
readVal program 0 _ = (program !) . (program !)
readVal program 1 _ = (program !)
readVal program 2 base = (program !) . (base +) . (program !)
readVal _ _ _ = undefined

readDst :: IntMap Int -> Int -> Int -> Int -> Int
readDst program 0 _ = (program !)
readDst program 2 base = (base +) . (program !)
readDst _ _ _ = undefined

step :: Vm -> Vm
step (Vm program (VmState alive i base input output)) =
  case x `mod` 100 of
    1 ->
      let val0 = readVal program m0 base (i + 1)
          val1 = readVal program m1 base (i + 2)
          dst = readDst program m2 base (i + 3)
       in Vm
            (insert dst (val0 + val1) program)
            (VmState alive (i + 4) base input output)
    2 ->
      let val0 = readVal program m0 base (i + 1)
          val1 = readVal program m1 base (i + 2)
          dst = readDst program m2 base (i + 3)
       in Vm
            (insert dst (val0 * val1) program)
            (VmState alive (i + 4) base input output)
    3 ->
      let dst = readDst program m0 base (i + 1)
       in Vm
            (insert dst (head input) program)
            (VmState alive (i + 2) base (tail input) output)
    4 ->
      let val = readVal program m0 base (i + 1)
       in Vm program $ VmState alive (i + 2) base input (val : output)
    5 ->
      let val0 = readVal program m0 base (i + 1)
          val1 = readVal program m1 base (i + 2)
       in Vm program $
            if val0 /= 0
              then VmState alive val1 base input output
              else VmState alive (i + 3) base input output
    6 ->
      let val0 = readVal program m0 base (i + 1)
          val1 = readVal program m1 base (i + 2)
       in Vm program $
            if val0 == 0
              then VmState alive val1 base input output
              else VmState alive (i + 3) base input output
    7 ->
      let val0 = readVal program m0 base (i + 1)
          val1 = readVal program m1 base (i + 2)
          dst = readDst program m2 base (i + 3)
       in Vm
            (insert dst (if val0 < val1 then 1 else 0) program)
            (VmState alive (i + 4) base input output)
    8 ->
      let val0 = readVal program m0 base (i + 1)
          val1 = readVal program m1 base (i + 2)
          dst = readDst program m2 base (i + 3)
       in Vm
            (insert dst (if val0 == val1 then 1 else 0) program)
            (VmState alive (i + 4) base input output)
    9 ->
      let val = readVal program m0 base (i + 1)
       in Vm program $ VmState alive (i + 2) (base + val) input output
    99 -> Vm program $ VmState False i base input output
    _ -> undefined
  where
    x = program ! i
    m0 = (x `div` 100) `mod` 10
    m1 = (x `div` 1000) `mod` 10
    m2 = (x `div` 10000) `mod` 10

patch :: IntMap Int -> [(Int, Int)] -> IntMap Int
patch program patches = fromList patches `union` program

run :: Vm -> Vm
run = head . dropWhile (getAlive . getState) . iterate step
