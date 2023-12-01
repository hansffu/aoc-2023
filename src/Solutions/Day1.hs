module Solutions.Day1 (solve1, solve2, part1) where

import Data.Char (intToDigit, isDigit)
import Data.List (elemIndex, find, isPrefixOf)

import Lib.TaskRunner (InputType (..), run)
import Lib.Utils (juxt, readInt)

solve1 :: IO Int
solve1 = run part1 $ Input 1

solve2 :: IO Int
solve2 = run part2 $ Input 1

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
