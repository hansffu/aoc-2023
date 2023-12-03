module Solutions.Day3 (day3) where

import Data.Char (isDigit)
import Data.List (nub)
import Lib.Solution (Solution (..))

data NumIndex = NumIndex Int [(Int, Int)] deriving (Show)

day3 :: Solution Int Int
day3 =
  Solution
    { day = 3
    , part1Solution = part1
    , part2Solution = part2
    }

part1 :: [String] -> IO Int
part1 input = do
  let numbers = zip [0 ..] input >>= uncurry parseRow
  let validNumbers = filter (isValid input) numbers
  return $ sum $ map (\(NumIndex i _) -> i) validNumbers

part2 :: [String] -> IO Int
part2 _ = return 0

isValid :: [String] -> NumIndex -> Bool
isValid input (NumIndex n indexes) = any (\x -> x /= '.' && not (isDigit x)) adjacent
 where
  adjacentIdx = adjacentIndexes (length $ head input) (length input) (NumIndex n indexes)
  adjacent = fmap (\(x, y) -> input !! y !! x) adjacentIdx

adjacentIndexes :: Int -> Int -> NumIndex -> [(Int, Int)]
adjacentIndexes maxX maxY (NumIndex _ indexes) =
  filter (`notElem` indexes) $
    filter (\(x, y) -> x >= 0 && y >= 0 && x < maxX && y < maxY) $
      nub (indexes >>= adjacentCell)
 where
  adjacentCell (a, b) = [(a + x, b + y) | x <- [-1, 0, 1], y <- [-1, 0, 1]]

parseRow :: Int -> String -> [NumIndex]
parseRow rowIndex line = map toNumIndex $ groupAdjacent nums
 where
  indexed = zip [0 ..] line
  nums = filter (isDigit . snd) indexed
  toNumIndex group = NumIndex (read $ map snd group) $ map (\x -> (fst x, rowIndex)) group

groupAdjacent :: [(Int, Char)] -> [[(Int, Char)]]
groupAdjacent = foldr go []
 where
  go num [] = [[num]]
  go num@(idx, _) (lastGroup@(lastNum@(lastIdx, _) : xs) : rest) =
    if lastIdx - idx == 1
      then (num : lastNum : xs) : rest
      else [num] : lastGroup : rest
  go _ ([] : _) = error "first group should never be empty"
