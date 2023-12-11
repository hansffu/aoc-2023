{-# LANGUAGE TupleSections #-}

module Solutions.Day11 (day11) where

import Data.List (transpose)
import Lib.Solution (Part, Solution (Solution))

day11 :: Solution Int Int
day11 = Solution 11 part1 part2

part1 :: Part Int
part1 input = return $ sum $ (\((i1, j1), (i2, j2)) -> abs (i1 - i2) + abs (j1 - j2)) <$> pairs
 where
  expanded = expandEmptySpace input
  indexes = findIndexes expanded
  pairs = findPairs indexes

part2 :: Part Int
part2 input = return $ sum $ (\((i1, j1), (i2, j2)) -> abs (i1 - i2) + abs (j1 - j2)) <$> pairs
 where
  emptyIs = findEmptyRows input
  emptyJs = findEmptyRows $ transpose input
  indexes = addEmptySpace emptyIs emptyJs 1000000 $ findIndexes input
  pairs = findPairs indexes

addEmptySpace :: [Int] -> [Int] -> Int -> [(Int, Int)] -> [(Int, Int)]
addEmptySpace emptyIs emptyJs extraSpace indexes =
  ( \(i, j) ->
      ( i + (extraSpace - 1) * length (filter (< i) emptyIs)
      , j + (extraSpace - 1) * length (filter (< j) emptyJs)
      )
  )
    <$> indexes

findEmptyRows :: [String] -> [Int]
findEmptyRows rows = fst <$> filter (all (== '.') . snd) (zip [0 ..] rows)

expandEmptySpace :: [String] -> [String]
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
