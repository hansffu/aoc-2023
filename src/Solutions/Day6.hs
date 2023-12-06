module Solutions.Day6 where

import Lib.Solution (Solution (Solution))
import Lib.TaskRunner (InputType (..), run)
import Lib.Utils (readInt)

day6 :: Solution Int Int
day6 = Solution 6 part1 part2

test :: IO Int
test = run part1 $ Sample 6

part1 :: [String] -> IO Int
part1 input = do
  let races = parseInput input
  return $ product $ raceScore <$> races

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
part2 input = return 0
