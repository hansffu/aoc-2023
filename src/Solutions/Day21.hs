module Solutions.Day21 (day21, test) where

import Data.Array ((!))
import Data.List (elemIndex, findIndex, nub)
import Data.List.Extra (firstJust)
import Data.Maybe (fromJust, isJust)
import GHC.Utils.Misc (nTimes)
import Lib.Solution (Part, Solution (..), todo)
import Lib.TaskRunner (InputType (..), run)
import Lib.Utils (Array2d, toArray2d)

day21 :: Solution Int Int
day21 = Solution 21 (part1 64) todo

test :: IO Int
test = run (part1 6) $ Sample 21

part1 :: Int -> Part Int
part1 steps input = return $ length $ nTimes steps expand [start]
 where
  start = findStart input
  inputArray = toArray2d input
  isPlot = isValidCoordinate inputArray
  expand = expandPlots isPlot

expandPlots :: ((Int, Int) -> Bool) -> [(Int, Int)] -> [(Int, Int)]
expandPlots isPlot prev = nub $ filter isPlot (prev >>= findNeighbours)

findNeighbours :: (Int, Int) -> [(Int, Int)]
findNeighbours (i, j) = [(i - 1, j), (i + 1, j), (i, j - 1), (i, j + 1)]

isValidCoordinate :: Array2d Char -> (Int, Int) -> Bool
isValidCoordinate input (i, j)
  | i < 0 || j < 0 || i > maxI || j > maxJ = False
  | otherwise = case input ! i ! j of
      'S' -> True
      '.' -> True
      '#' -> False
      _ -> error "invalid input"
 where
  (maxI, maxJ) = (length input - 1, length (input ! 0) - 1)

findStart :: [[Char]] -> (Int, Int)
findStart input = (i, j)
 where
  js = elemIndex 'S' <$> input
  i = fromJust $ findIndex isJust js
  j = fromJust $ firstJust id js
