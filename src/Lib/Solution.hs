module Lib.Solution (Solution (..), solve) where

import Lib.TaskRunner (InputType (..), readInput)

data (Show a, Show b) => Solution a b = Solution
  { day :: Int
  , part1Solution :: [String] -> IO a
  , part2Solution :: [String] -> IO b
  }

solve :: (Show a, Show b) => Solution a b -> IO (a, b)
solve solution = do
  input <- readInput $ Input $ day solution
  p1 <- part1Solution solution input
  p2 <- part2Solution solution input
  return (p1, p2)
