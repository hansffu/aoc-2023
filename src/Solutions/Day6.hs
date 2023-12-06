module Solutions.Day6 (day6) where

import Control.Monad (join)
import Lib.Solution (Solution (Solution))
import Lib.Utils (readInt)

day6 :: Solution Int Int
day6 = Solution 6 part1 part2

part1 :: [String] -> IO Int
part1 = return . product . (raceScore <$>) . parseInput

raceScore :: (Int, Int) -> Int
raceScore (duration, record) = length $ fst <$> filter (\(_, dis) -> dis > record) (take duration distanceTable)
 where
  distanceTable = [(ms, calculateDistance duration ms) | ms <- [0 ..]]

parseInput :: [String] -> [(Int, Int)]
parseInput input = zip durations distances
 where
  parseLine line = readInt <$> drop 1 (words line)
  durations = parseLine $ head input
  distances = parseLine $ input !! 1

calculateDistance :: Int -> Int -> Int
calculateDistance duration ms = (duration - ms) * ms

part2 :: [String] -> IO Int
part2 = return . raceScore . parseInput2

parseInput2 :: [String] -> (Int, Int)
parseInput2 input = (duration, distance)
 where
  parseLine = readInt . join . drop 1 . words
  duration = parseLine $ head input
  distance = parseLine $ input !! 1
