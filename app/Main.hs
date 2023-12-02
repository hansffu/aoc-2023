module Main where

import Lib.Solution (Solution (day), solve)
import Solutions.Day1 (day1)

main :: IO ()
main = run day1

run :: (Show a, Show b) => Solution a b -> IO ()
run solution = do
  putStrLn $ "Day " <> show (day solution)
  (part1, part2) <- solve day1
  putStrLn $ "Part 1: " <> show part1
  putStrLn $ "Part 2: " <> show part2
