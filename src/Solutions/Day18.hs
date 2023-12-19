{-# LANGUAGE TupleSections #-}

module Solutions.Day18 (day18, test) where

import Data.Either.Utils (fromRight)

import Lib.Parser (intP, parseAll)
import Lib.Solution (Part, Solution (Solution), todo)
import Lib.TaskRunner (InputType (..), run)

import Data.List (groupBy, nub, sort)
import Data.Tuple.Extra (fst3)
import qualified Text.Parsec as P

day18 :: Solution Int Int
day18 = Solution 18 part1 todo

test :: IO Int
test = run part1 $ Input 18

part1 :: Part Int
part1 input = return $ sum $ uncurry calcLine <$> matched
 where
  parsed = parseInput input
  corners = findCorners parsed
  trenchCoords = calcTrenchCoords (0, 0) parsed
  groupedTrenchCoords = groupByI' trenchCoords
  matched = match corners <$> groupedTrenchCoords

match :: [Corner] -> [Trench] -> ([Trench], [Corner])
match _ [] = error "should be no empty groups"
match corners ts@(((ti, _), _) : _) = (ts, filter (\(ci, _, _, _) -> ci == ti) corners)

calcLine :: [Trench] -> [Corner] -> Int
calcLine trenches' corners' = calc flipOnPairs + sumHorizontalTrenches
 where
  validCorners = (\(_, j, _, _) -> j) <$> filter isValidCorner corners'
  invalidCorners = (\(_, j, _, _) -> j) <$> filter (not . isValidCorner) corners'
  verticalTrenches = filter (`notElem` invalidCorners) $ (\((_, j), _) -> j) <$> filter (\(_, dir) -> dir == 'U' || dir == 'D') trenches'
  sumHorizontalTrenches = length $ filter notInFlipPairs trenches'
  flipOn = sort $ nub $ validCorners <> verticalTrenches
  flipOnPairs = pairs flipOn
  notInFlipPairs ((_, j), _) = not $ any (\(a, b) -> j >= a && j <= b) flipOnPairs
  calc ((x, y) : xs) = y - x + 1 + calc xs
  calc [] = 0

pairs :: [Int] -> [(Int, Int)]
pairs [] = []
pairs [_] = []
pairs (x : y : xs) = (x, y) : pairs xs

type Instruction = (Char, Int, String)
type Dir = Char
data CornerType = SW | NW | NE | SE | StartEnd deriving (Show)
type Corner = (Int, Int, Dir, Dir)
type Coord = (Int, Int)
type Trench = (Coord, Dir)

groupByI' :: [Trench] -> [[Trench]]
groupByI' corners = groupBy (\((a, _), _) ((b, _), _) -> a == b) $ sort corners

findCorners :: [Instruction] -> [Corner]
findCorners instructions = corners'
 where
  lastDir = fst3 $ last instructions
  start = (0, 0, 'S', lastDir)
  corners' = findCorners' start instructions

isValidCorner :: Corner -> Bool
isValidCorner (_, _, 'R', 'D') = True
isValidCorner (_, _, 'U', 'L') = True
isValidCorner (_, _, 'L', 'D') = True
isValidCorner (_, _, 'U', 'R') = True
isValidCorner _ = False

findCorners' :: Corner -> [Instruction] -> [Corner]
findCorners' prevCorner [] = [prevCorner]
findCorners' (i, j, _, inDir) ((outDir, len, _) : inss) = prevCorner : findCorners' nextCorner inss
 where
  prevCorner = (i, j, inDir, outDir)
  nextCorner = case outDir of
    'R' -> (i, j + len, inDir, outDir)
    'L' -> (i, j - len, inDir, outDir)
    'U' -> (i - len, j, inDir, outDir)
    'D' -> (i + len, j, inDir, outDir)
    _ -> error "invalid input"

calcTrenchCoords :: Coord -> [Instruction] -> [Trench]
calcTrenchCoords _ [] = []
calcTrenchCoords (i, j) ((dir, len, _) : inss) = tail ((,dir) <$> newCoords) <> calcTrenchCoords (last $ newCoords) inss
 where
  newCoords = case dir of
    'R' -> [(i, j + j') | j' <- [0 .. len]]
    'L' -> [(i, j - j') | j' <- [0 .. len]]
    'U' -> [(i - i', j) | i' <- [0 .. len]]
    'D' -> [(i + i', j) | i' <- [0 .. len]]
    _ -> error "invalid input"

parseInput :: [String] -> [Instruction]
parseInput = fromRight . parseAll parse'
 where
  parse' = do
    dir <- P.anyChar <* P.spaces
    len <- intP <* P.spaces
    color <- P.string "(#" *> P.many P.hexDigit <* P.char ')'
    return (dir, len, color)
