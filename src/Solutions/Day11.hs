{-# LANGUAGE TupleSections #-}

module Solutions.Day11 where

import Data.List (transpose)
import Data.List.Utils (join)
import Lib.Solution (Part, Solution (Solution))
import Lib.TaskRunner (InputType (..), run)

day11 :: Solution Int Int
day11 = Solution 1 part1 part2

test :: IO Int
test = run part1 $ Input 11

part1 :: Part Int
part1 input = do
  return $ sum $ (\((i1, j1), (i2, j2)) -> abs (i1 - i2) + abs (j1 - j2)) <$> pairs
 where
  expanded = expandEmptySpace input
  indexes = findIndexes expanded
  pairs = findPairs indexes

part2 :: Part Int
part2 input = return 0

expandEmptySpace :: [[Char]] -> [[Char]]
expandEmptySpace space = transpose (transpose (space >>= expandRow) >>= expandRow)
 where
  expandRow row = if all (== '.') row then [row, row] else [row]

findIndexes :: [String] -> [(Int, Int)]
findIndexes rows = indexed >>= (\(i, row) -> row >>= (\(j, c) -> ([(i, j) | c == '#'])))
 where
  indexed = zip [0 ..] $ zip [0 ..] <$> rows

findPairs :: (Eq a) => [a] -> [(a, a)]
findPairs [] = []
findPairs [_] = []
findPairs (x : xs) = ((x,) <$> xs) `mappend` findPairs xs

prettyPrint :: [String] -> IO ()
prettyPrint = putStrLn . join "\n"
