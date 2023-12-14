{-# LANGUAGE TupleSections #-}

module Solutions.Day14 (day14) where

import Data.List (elemIndex, sortBy, transpose)
import Data.Ord (comparing)
import qualified Data.Ord

import Data.List.Extra (firstJust)
import Data.Maybe (fromJust)
import GHC.Utils.Misc (nTimes)
import Lib.Solution (Part, Solution (Solution))

day14 :: Solution Int Int
day14 = Solution 14 part1 part2

part1 :: Part Int
part1 input = return $ sum $ fst <$> filter ((== 'O') . snd) (afterFalling >>= (zip [1 ..] . reverse))
 where
  afterFalling = fallLeft <$> rotateCCW input

part2 :: Part Int
part2 input = return $ calculateLoad $ spins !! firstOccurence
 where
  calculateLoad :: [String] -> Int
  calculateLoad rocks = sum $ fst <$> filter ((== 'O') . snd) (rotateCW rocks >>= (zip [1 ..] . reverse))
  rotatedInput = rotateCCW $ rotateCCW input
  spins = scanl (\x _ -> spin x) rotatedInput ([0 .. 1000000000] :: [Int])
  dups = (\(i, x) -> (,i) <$> x `elemIndex` take (i - 1) spins) <$> zip [0 ..] spins
  firstCycle = fromJust $ firstJust id dups
  cycleLength = snd firstCycle - fst firstCycle
  firstOccurence = (1000000000 - fst firstCycle) `mod` cycleLength + fst firstCycle

rotateCCW :: [String] -> [String]
rotateCCW = transpose . (reverse <$>)

rotateCW :: [String] -> [String]
rotateCW = transpose . reverse

spin :: [String] -> [String]
spin = nTimes 4 ((fallLeft <$>) . rotateCW)

fallLeft :: String -> String
fallLeft [] = []
fallLeft ('#' : xs) = '#' : fallLeft xs
fallLeft s = loose ++ fallLeft rest
 where
  loose = sortBy (comparing Data.Ord.Down) $ takeWhile (/= '#') s
  rest = dropWhile (/= '#') s
