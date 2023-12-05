{-# LANGUAGE TupleSections #-}

module Solutions.Day5 where

import Data.Ix (inRange)
import Data.List.Split (chunksOf, splitOn)

import Lib.Solution (Solution (Solution, day, part1Solution, part2Solution))
import Lib.TaskRunner (InputType (..), run)
import Lib.Utils (readInt)

type RangeMapping = (Int, Int, Int)

day5 :: Solution Int Int
day5 =
  Solution
    { day = 5
    , part1Solution = part1
    , part2Solution = part2
    }

test :: IO Int
test = run part2 $ Sample 5

part1 :: [String] -> IO Int
part1 input = do
  let seeds = readInt <$> tail (words $ head input)
  let converter = getConverter input
  return $ minimum (converter <$> seeds)

part2 :: [String] -> IO Int
part2 input = do
  let seedRanges = (\x -> (head x, head x + x !! 1)) <$> chunksOf 2 (readInt <$> tail (words $ head input)) >>= (\(a, b) -> [a..b])
  let seeds = readInt <$> tail (words $ head input)
  -- print seedRanges
  -- print $ rangeDefs input
  -- print $ maximum seeds
  let converter = getConverter input
  -- return $ minimum (converter <$> (take 100000 seeds))
  -- let a = foldr1 min (converter <$> seeds)
  let a = foldl1 (\acc cur -> min acc $ converter cur) seedRanges
  return a

rangeDefs :: [String] -> [[RangeMapping]]
rangeDefs input = fmap (head . toRangeMappings . (readInt <$>) . words) . tail <$> splitOn [""] (drop 2 input)

getConverter :: [String] -> (Int -> Int)
getConverter input = foldl (.) id (reverse (toDestination <$> rangeDefs input))

toRangeMappings :: [Int] -> [RangeMapping]
toRangeMappings (a : b : c : xs) = (a, b, c) : toRangeMappings xs
toRangeMappings _ = []

toDestination :: [RangeMapping] -> Int -> Int
toDestination [] sourceNum = sourceNum
toDestination ((destinationStart, sourceStart, rangeLength) : xs) sourceNum
  | inRange (sourceStart, sourceStart + rangeLength - 1) sourceNum = destinationStart - sourceStart + sourceNum
  | otherwise = toDestination xs sourceNum

testdata :: [RangeMapping]
testdata = [(50, 98, 2), (52, 50, 48)]
