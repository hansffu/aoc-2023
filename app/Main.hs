module Main where

import Lib.Solution (Solution (day), solve)
import Solutions.Day1 (day1)
import Solutions.Day2 (day2)
import Solutions.Day3 (day3)
import Solutions.Day4 (day4)
import Solutions.Day5 (day5)
import Solutions.Day6 (day6)
import Solutions.Day7 (day7)

currentDay :: Int
currentDay = 7

runAll :: Bool
runAll = False

main :: IO ()
main = mapM_ run (filter (\s -> runAll || day s == currentDay) solutions)
 where
  solutions = [day1, day2, day3, day4, day5, day6, day7]

run :: (Show a, Show b) => Solution a b -> IO ()
run solution = do
  putStrLn $ "Day " <> show (day solution)
  (part1, part2) <- solve solution
  putStrLn $ "Part 1: " <> show part1
  putStrLn $ "Part 2: " <> show part2
