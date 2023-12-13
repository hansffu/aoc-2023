module Solutions.Day13 (day13, test) where

import Data.List (transpose)
import Data.List.Extra (splitOn)
import Data.List.Ordered (isect)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Tuple.Extra (first)
import Lib.Solution (Part, Solution (Solution))
import Lib.TaskRunner (InputType (..), run)

day13 :: Solution Int Int
day13 = Solution 13 part1 part2

test :: IO Int
test = run part1 $ Sample 13

part1 :: Part Int
part1 = return . sum . (calculateGroupScore <$>) . parseInput
 where
  calculateGroupScore group = res
   where
    res = fromMaybe 0 $ firstJust horizontal ((* 100) <$> vertical)
    horizontal = listToMaybe $ foldr1 isect $ symmetricIndices <$> group
    vertical = listToMaybe $ foldr1 isect $ symmetricIndices <$> transpose group

part2 :: Part Int
part2 = const $ return 0

symmetricIndices :: String -> [Int]
symmetricIndices s = filter (checkIndex s) [1 .. length s - 1]
 where
  checkIndex input n = and $ uncurry (zipWith (==)) $ first reverse $ splitAt n input

parseInput :: [String] -> [[String]]
parseInput = splitOn [""]

firstJust :: Maybe a -> Maybe a -> Maybe a
firstJust (Just a) _ = Just a
firstJust Nothing b = b
