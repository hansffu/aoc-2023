module Solutions.Day5 where

import Data.Ix (inRange)
import Data.List.Split (splitOn)
import Lib.Solution (Solution (Solution, day, part1Solution, part2Solution))
import Lib.TaskRunner (InputType (..), run)
import Lib.Utils (readInt)

type RangeMapping = (Int, Int, Int)

day5 :: Solution Int Int
day5 =
  Solution
    { day = 5
    , part1Solution = part1
    , part2Solution = return . const 0
    }

test :: IO Int
test = run part1 $ Sample 5

part1 :: [String] -> IO Int
part1 input = do
  let seeds = readInt <$> tail (words $ head input)
  let rangeDefs = fmap (head . toRangeMappings . (readInt <$>) . words) . tail <$> splitOn [""] (drop 2 input)
  let converters = toDestination <$> rangeDefs
  let converter = foldl (.) id (reverse converters)
  return $ minimum (converter <$> seeds)

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
