{-# LANGUAGE TupleSections #-}

module Solutions.Day13 (day13, test) where

import Data.List (transpose)
import Data.List.Extra (splitOn)
import Data.List.Ordered (isect)
import Data.Maybe (fromJust, isJust, listToMaybe)
import Data.Tuple.Extra (first)
import GHC.Data.Maybe (firstJusts)
import Lib.Solution (Part, Solution (Solution))
import Lib.TaskRunner (InputType (..), run)
import Lib.Utils (index2d)

day13 :: Solution Int Int
day13 = Solution 13 part1 part2

test :: IO Int
test = run part2 $ Input 13

part1 :: Part Int
part1 = return . sum . (snd . fromJust . calculateGroupScore (const True) (const True) <$>) . parseInput

data Dir = H | V deriving (Show, Eq)

calculateGroupScore :: (Int -> Bool) -> (Int -> Bool) -> [[Char]] -> Maybe (Dir, Int)
calculateGroupScore hFilter vFilter group = firstJusts [(H,) <$> horizontal, (V,) . (* 100) <$> vertical]
 where
  horizontal = listToMaybe $ foldr1 isect $ symmetricIndices hFilter <$> group
  vertical = listToMaybe $ foldr1 isect $ symmetricIndices vFilter <$> transpose group

part2 :: Part Int
part2 input = do
  results <- mapM part2' groups
  return $ sum results
 where
  groups = parseInput input

part2' :: [String] -> IO Int
part2' group = do
  let (d, n) = fromJust $ calculateGroupScore (const True) (const True) group
  let hFilter = if d == H then (/= n) else const True
  let vFilter = if d == V then (/= n `div` 100) else const True
  return $ snd $ fromJust $ head $ filter isJust $ calculateGroupScore hFilter vFilter <$> flipped
 where
  flipped = flip' <$> variations
  variations = [(i, j) | i <- [0 .. length group - 1], j <- [0 .. length (head group) - 1]]
  flip' idx = (flip'' idx <$>) <$> index2d group
  flip'' idx (idx', c)
    | idx == idx' = if c == '#' then '.' else '#'
    | otherwise = c

symmetricIndices :: (Int -> Bool) -> String -> [Int]
symmetricIndices extraFilter s = filter (\i -> extraFilter i && checkIndex s i) [1 .. length s - 1]
 where
  checkIndex input n = and $ uncurry (zipWith (==)) $ first reverse $ splitAt n input

parseInput :: [String] -> [[String]]
parseInput = splitOn [""]
