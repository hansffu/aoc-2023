module Solutions.Day1 (day1) where

import Data.Char (intToDigit, isDigit)
import Data.List (elemIndex, find, isPrefixOf)

import Lib.Solution
import Lib.Utils (juxt, readInt)

day1 :: Solution Int Int
day1 = Solution 1 part1 part2

part1 :: [String] -> IO Int
part1 = return . sum . map parseNumber

part2 :: [String] -> IO Int
part2 = return . sum . map (parseNumber . cleanNumbers)

parseNumber :: String -> Int
parseNumber = readInt . juxt [head, last] . filter isDigit

cleanNumbers :: String -> String
cleanNumbers [] = []
cleanNumbers str@(x : xs) = case cleaned of
  Just d -> d : cleanNumbers xs
  Nothing -> cleanNumbers xs
 where
  nums = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
  cleaned
    | isDigit x = Just x
    | otherwise = intToDigit . succ <$> (find (`isPrefixOf` str) nums >>= (`elemIndex` nums))
