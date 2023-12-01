module Main where

import Solutions.Day1 (part1)
import Test.Tasty
import Test.Tasty.HUnit
import Utils (testSolution)

main :: IO ()
main = defaultMain day1Tests

day1Tests :: TestTree
day1Tests =
    testGroup
        "Day 1"
        [ testCase "part 1"
            $ do
                result <- testSolution part1 1
                assertEqual "" result 7
                -- @?= 7
        ]

