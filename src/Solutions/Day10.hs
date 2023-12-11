module Solutions.Day10 where

import Control.Monad (mfilter)
import Data.Array (Array, bounds, listArray, (!))
import Data.Maybe (fromJust, isJust)
import Lib.Solution (Part, Solution (Solution))
import Lib.TaskRunner (InputType (..), run)

day10 :: Solution Int Int
day10 = Solution 10 part1 part2

test :: IO Int
test = run part1 $ Sample 10

part1 :: Part Int
part1 input = return $ if odd loopLength then (loopLength - 1) `div` 2 else loopLength `div` 2
 where
  start = findStart input
  inputArr = toArray2d input
  secondPipes = startingCells input start
  loop = buildLoop inputArr start (head secondPipes)
  loopLength = length (takeWhile (/= start) (tail loop)) + 1

buildLoop :: Array2d Char -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
buildLoop rows start firstPipe = loop
 where
  loop = start : firstPipe : zipWith nextPipe loop (tail loop)
  nextPipe lastPipe currentPipe@(i, j) =
    head $
      filter
        (/= lastPipe)
        ( case rows ! i ! j of
            '|' -> [(i - 1, j), (i + 1, j)]
            '-' -> [(i, j - 1), (i, j + 1)]
            'L' -> [(i - 1, j), (i, j + 1)]
            'J' -> [(i - 1, j), (i, j - 1)]
            '7' -> [(i + 1, j), (i, j - 1)]
            'F' -> [(i + 1, j), (i, j + 1)]
            'S' -> [firstPipe]
            _ -> error $ "on the ground " <> show currentPipe
        )

neighbours :: Array2d Char -> (Int, Int) -> [(Int, Int)]
neighbours rows (i, j) =
  filter
    inBounds
    [ (i - 1, j)
    , (i + 1, j)
    , (i, j - 1)
    , (i, j + 1)
    ]
 where
  iBound = snd $ bounds rows
  jBound = snd $ bounds (rows ! 0)
  inBounds (i', j') = i' >= 0 && i' < iBound && j' >= 0 && j' < jBound

part2 :: Part Int
part2 = return . const 0

findStart :: [String] -> (Int, Int)
findStart rows = head $ indexed >>= (\(i, row) -> row >>= (\(j, c) -> ([(i, j) | c == 'S'])))
 where
  indexed = zip [0 ..] $ zip [0 ..] <$> rows

startingCells :: [String] -> (Int, Int) -> [(Int, Int)]
startingCells rows (i, j) =
  fromJust $
    sequence $
      filter
        isJust
        [ mfilter (isOneOf "|7F") $ Just (i - 1, j)
        , mfilter (isOneOf "|JL") $ Just (i + 1, j)
        , mfilter (isOneOf "-LF") $ Just (i, j - 1)
        , mfilter (isOneOf "-J7") $ Just (i, j + 1)
        ]
 where
  inBounds (i', j') = i' >= 0 && i' < length rows && j' >= 0 && j' < length (head rows)
  isOneOf xs (i', j') = inBounds (i', j') && rows !! i' !! j' `elem` xs

type Array2d a = Array Int (Array Int a)

toArray2d :: [[a]] -> Array2d a
toArray2d rows = listArray (0, length rows - 1) $ listArray (0, length (head rows) - 1) <$> rows
