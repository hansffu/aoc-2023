module Main where

import Lib.Solution (Solution (day), solve)
import Solutions.Day2 (day2)

main :: IO ()
main = run day2

run :: (Show a, Show b) => Solution a b -> IO ()
run solution = do
  putStrLn $ "Day " <> show (day solution)
  (part1, part2) <- solve solution
  putStrLn $ "Part 1: " <> show part1
  putStrLn $ "Part 2: " <> show part2
