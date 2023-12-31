module Main where

import Lib.Solution (Solution (part1Solution, part2Solution))
import Solutions.Day1 (day1)
import Solutions.Day2 (day2)
import Solutions.Day3 (day3)
import Solutions.Day4 (day4)
import Test.Tasty
import Test.Tasty.HUnit
import Utils (testSolution, testSolution')

main :: IO ()
main = defaultMain day4Tests

day1Tests :: TestTree
day1Tests =
  testGroup
    "Day 1"
    [ testCase "part 1" $
        do
          result <- testSolution' (part1Solution day1) "1.1"
          assertEqual "" 142 result
    , testCase "part 2" $
        do
          result <- testSolution (part2Solution day1) 1
          assertEqual "" 281 result
    ]

day2Tests :: TestTree
day2Tests =
  testGroup
    "Day 2"
    [ testCase "part 1" $
        do
          result <- testSolution (part1Solution day2) 2
          assertEqual "" 8 result
    , testCase "part 2" $
        do
          result <- testSolution (part2Solution day2) 2
          assertEqual "" 2286 result
    ]

day3Tests :: TestTree
day3Tests =
  testGroup
    "Day 3"
    [ testCase "part 1" $
        do
          result <- testSolution (part1Solution day3) 3
          assertEqual "" 4361 result
    , testCase "part 2" $
        do
          result <- testSolution (part2Solution day3) 3
          assertEqual "" 467835 result
    ]

day4Tests :: TestTree
day4Tests =
  testGroup
    "Day 4"
    [ testCase "part 1" $
        do
          result <- testSolution (part1Solution day4) 4
          assertEqual "" 13 result
    , testCase "part 2" $
        do
          result <- testSolution (part2Solution day4) 4
          assertEqual "" 30 result
    ]
