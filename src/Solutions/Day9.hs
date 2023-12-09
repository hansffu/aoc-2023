module Solutions.Day9 where

import Lib.Solution (Solution (..))
import Lib.TaskRunner (InputType (..), run)

day9 :: Solution Int Int
day9 = Solution 9 part1 part2

test :: IO Int
test = run part1 $ Input 9

part1 :: [String] -> IO Int
part1 input' = return $ sum (calcNext . diffsTo0 <$> input)
 where
  input = parseInput input'

calcNext :: [[Int]] -> Int
calcNext = foldl (\prev cur -> head cur + prev) 0

diffsTo0 :: [Int] -> [[Int]]
diffsTo0 input
  | all (== 0) input = []
  | otherwise = input : diffsTo0 diff
 where
  diff = diffs input

diffs :: [Int] -> [Int]
diffs [] = []
diffs [_] = []
diffs (x : y : xs) = (x - y) : diffs (y : xs)

part2 :: [String] -> IO Int
part2 input = return 0

parseInput :: [String] -> [[Int]]
parseInput = (parseLine <$>)
 where
  parseLine = (read <$>) . reverse . words
