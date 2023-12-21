module Main where

import Lib.Solution (Solution (day), solve)
import Solutions.Day1 (day1)
import Solutions.Day10 (day10)
import Solutions.Day11 (day11)
import Solutions.Day12 (day12)
import Solutions.Day13 (day13)
import Solutions.Day14 (day14)
import Solutions.Day15 (day15)
import Solutions.Day16 (day16)
import Solutions.Day18 (day18)
import Solutions.Day19 (day19)
import Solutions.Day2 (day2)
import Solutions.Day20 (day20)
import Solutions.Day3 (day3)
import Solutions.Day4 (day4)
import Solutions.Day5 (day5)
import Solutions.Day6 (day6)
import Solutions.Day7 (day7)
import Solutions.Day8 (day8)
import Solutions.Day9 (day9)

currentDay :: Int
currentDay = 20

runAll :: Bool
runAll = False

main :: IO ()
main = mapM_ run (filter (\s -> runAll || day s == currentDay) solutions)
 where
  solutions =
    [ day1
    , day2
    , day3
    , day4
    , day5
    , day6
    , day7
    , day8
    , day9
    , day10
    , day11
    , day12
    , day13
    , day14
    , day15
    , day16
    , day18
    , day19
    , day20
    ]

run :: (Show a, Show b) => Solution a b -> IO ()
run solution = do
  putStrLn $ "Day " <> show (day solution)
  (part1, part2) <- solve solution
  putStrLn $ "Part 1: " <> show part1
  putStrLn $ "Part 2: " <> show part2
