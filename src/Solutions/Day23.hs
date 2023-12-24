module Solutions.Day23 (day23, test) where

import qualified Data.Map as M
import Data.Maybe (catMaybes)
import qualified Data.Set as S
import Lib.Solution (Part, Solution (Solution), todo)
import Lib.TaskRunner (InputType (..), run)
import Lib.Utils (applyT2)

day23 :: Solution Int Int
day23 = Solution 23 part1 todo

test :: IO Int
test = run part1 $ Sample 23

part1 :: Part Int
part1 input = return $ longestPath groundMap $ getCoord start
 where
  groundList = parseInput input
  groundMap = M.fromList $ applyT2 (getCoord, id) <$> groundList
  start = head groundList

type Coord = (Int, Int)
data Dir = U | D | L | R deriving (Show)
data Ground = Path Coord | Slope Coord Dir deriving (Show)

longestPath :: M.Map Coord Ground -> Coord -> Int
longestPath groundMap = go (-1) S.empty
 where
  go dist visited current
    | alreadyVisited = dist
    | otherwise = maximum $ go (dist + 1) (S.insert current visited) <$> neighbors
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
