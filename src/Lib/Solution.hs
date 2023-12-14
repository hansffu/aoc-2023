module Lib.Solution (Solution (..), Part, solve, testSolution, todo) where

import Lib.TaskRunner (InputType (..), readInput)

type Part a = [String] -> IO a

todo :: [String] -> IO Int
todo = const $ return 0

data (Show a, Show b) => Solution a b = Solution
  { day :: Int
  , part1Solution :: [String] -> IO a
  , part2Solution :: [String] -> IO b
  }

solve :: (Show a, Show b) => Solution a b -> IO (a, b)
solve solution = run solution Input

testSolution :: (Show a, Show b) => Solution a b -> IO (a, b)
testSolution solution = run solution Sample

run :: (Show a, Show b) => Solution a b -> (Int -> InputType) -> IO (a, b)
run solution inputType = do
  input <- readInput $ inputType $ day solution
  p1 <- part1Solution solution input
  p2 <- part2Solution solution input
  return (p1, p2)
