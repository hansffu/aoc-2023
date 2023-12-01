-- | yo
module Solutions.Day1 (solve1, solve2, part1) where

import Lib.TaskRunner (InputType (..), run)

solve1 :: IO Int
solve1 = run part1 $ Input 1

solve2 :: IO Int
solve2 = run part2 $ Input 1

part1 :: [String] -> IO Int
part1 input = return $ increases $ map readInt input

part2 :: [String] -> IO Int
part2 input = return $ increases sw
 where
  depths = map readInt input
  sw = zipWith3 (\x y z -> x + y + z) (drop 2 depths) (tail depths) depths

readInt :: String -> Int
readInt = read

increases :: [Int] -> Int
increases [] = 0
increases [_x] = 0
increases (x : y : xs)
  | x < y = 1 + increases (y : xs)
  | otherwise = increases (y : xs)
