{-# LANGUAGE LambdaCase #-}

module Solutions.Day23 (day23, test) where

import Control.Parallel.Strategies (parMap, rdeepseq)
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import qualified Data.Set as S
import Lib.Solution (Part, Solution (Solution), todo)
import Lib.TaskRunner (InputType (..), run)
import Lib.Utils (applyT2)

day23 :: Solution Int Int
day23 = Solution 23 part1 part2

test :: IO Int
test = run part2 $ Sample 23

part1 :: Part Int
part1 input = return $ longestPath groundMap (getCoord end) (getCoord start)
 where
  groundList = parseInput input
  groundMap = M.fromList $ applyT2 (getCoord, id) <$> groundList
  start = head groundList
  end = last groundList

part2 :: Part Int
part2 input = return $ longestPath groundMap (getCoord end) (getCoord start)
 where
  groundList = (\case Slope c _ -> Path c; p -> p) <$> parseInput input
  groundMap = M.fromList $ applyT2 (getCoord, id) <$> groundList
  start = head groundList
  end = last groundList

type Coord = (Int, Int)
data Dir = U | D | L | R deriving (Show)
data Ground = Path Coord | Slope Coord Dir deriving (Show)

longestPath :: M.Map Coord Ground -> Coord -> Coord -> Int
longestPath groundMap end = fromMaybe 0 . go 0 S.empty
 where
  go :: Int -> S.Set (Int, Int) -> Coord -> Maybe Int
  go dist visited current
    | current == end = Just dist
    | alreadyVisited = Nothing
    | otherwise = Just $ foldr max 0 $ catMaybes $ parMap rdeepseq (go (dist + 1) (S.insert current visited)) neighbors
   where
    alreadyVisited = S.member current visited
    neighbors = getNeighbours groundMap current
getCoord :: Ground -> Coord
getCoord (Path c) = c
getCoord (Slope c _) = c

getNeighbours :: M.Map Coord Ground -> Coord -> [Coord]
getNeighbours groundMap coord = filter (`M.member` groundMap) $ getNeighbours' ((M.!) groundMap coord)
 where
  getNeighbours' :: Ground -> [Coord]
  getNeighbours' (Path (i, j)) = [(i - 1, j), (i + 1, j), (i, j - 1), (i, j + 1)]
  getNeighbours' (Slope (i, j) U) = [(i - 1, j)]
  getNeighbours' (Slope (i, j) D) = [(i + 1, j)]
  getNeighbours' (Slope (i, j) L) = [(i, j - 1)]
  getNeighbours' (Slope (i, j) R) = [(i, j + 1)]

parseInput :: [String] -> [Ground]
parseInput input = catMaybes $ do
  (i, row) <- zip [0 ..] input
  (j, ground) <- zip [0 ..] row
  let c = (i, j)
  return $ case ground of
    '.' -> Just $ Path c
    '^' -> Just $ Slope c U
    'v' -> Just $ Slope c D
    '>' -> Just $ Slope c R
    '<' -> Just $ Slope c L
    _ -> Nothing
