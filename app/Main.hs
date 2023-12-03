module Main where

import Lib.Solution (Solution (day), solve)
import Solutions.Day1 (day1)
import Solutions.Day2 (day2)
import Solutions.Day3 (day3)

currentDay :: Int
currentDay = 3

main :: IO ()
main = mapM_ run (filter (\s -> day s == currentDay) solutions)
 where
  solutions = [day1, day2, day3]

run :: (Show a, Show b) => Solution a b -> IO ()
run solution = do
  putStrLn $ "Day " <> show (day solution)
  (part1, part2) <- solve solution
  putStrLn $ "Part 1: " <> show part1
  putStrLn $ "Part 2: " <> show part2
