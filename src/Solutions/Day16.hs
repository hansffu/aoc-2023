module Solutions.Day16 (day16, test) where

import Control.Applicative ((<|>))

import Control.Parallel.Strategies (parMap, rdeepseq)
import Data.Either.Utils (fromRight)
import Data.List (nub)

import Lib.Dfs (dfs)
import Lib.Parser (parseAll)
import Lib.Solution (Part, Solution (..))
import Lib.TaskRunner (InputType (..), run)
import qualified Text.Parsec as P

day16 :: Solution Int Int
day16 = Solution 16 part1 part2

test :: IO Int
test = run part2 $ Sample 16

part1 :: Part Int
part1 input = return $ length $ nub $ coord <$> visitedState
 where
  tiles = parseInput input
  visitedState = goLights tiles $ Step (0, 0) E

part2 :: Part Int
part2 input = return $ maximum $ parMap rdeepseq go startingPositions
 where
  go startingPosition = length $ nub $ coord <$> goLights tiles startingPosition
  tiles = parseInput input
  startingPositions =
    ((\i -> Step (i, 0) E) <$> [0 .. length input - 1])
      <> ((\i -> Step (i, length (head input) - 1) W) <$> [0 .. length input - 1])
      <> ((\j -> Step (0, j) S) <$> [0 .. length (head input) - 1])
      <> ((\j -> Step (length input - 1, j) N) <$> [0 .. length (head input) - 1])

goLights :: [[TileType]] -> Step -> [Step]
goLights tiles = dfs (nextSteps tiles)

data Direction = E | W | N | S deriving (Show, Eq)
data TileType = Empty | VSplit | HSplit | DiagLR | DiagRL
type Coord = (Int, Int)
data Step = Step {coord :: Coord, direction :: Direction} deriving (Show, Eq)

nextSteps :: [[TileType]] -> Step -> [Step]
nextSteps tiles (Step (i, j) dir) = filter inBounds $ (\d -> Step (coordTo d) d) <$> getDirections dir (tiles !! i !! j)
 where
  inBounds (Step (i', j') _) = i' >= 0 && i' < length tiles && j' >= 0 && j' < length (head tiles)
  coordTo :: Direction -> Coord
  coordTo E = (i, j + 1)
  coordTo W = (i, j - 1)
  coordTo N = (i - 1, j)
  coordTo S = (i + 1, j)

getDirections :: Direction -> TileType -> [Direction]
getDirections d Empty = [d]
getDirections d VSplit = if isVertical d then [N, S] else [d]
getDirections d HSplit = if isVertical d then [d] else [E, W]
getDirections E DiagLR = [S]
getDirections S DiagLR = [E]
getDirections W DiagLR = [N]
getDirections N DiagLR = [W]
getDirections E DiagRL = [N]
getDirections S DiagRL = [W]
getDirections W DiagRL = [S]
getDirections N DiagRL = [E]

isVertical :: Direction -> Bool
isVertical d = d `elem` [E, W]

instance Show TileType where
  show Empty = "."
  show VSplit = "|"
  show HSplit = "-"
  show DiagLR = "\\"
  show DiagRL = "/"

instance Ord Step where
  compare a b = compare (show a) (show b)

parseInput :: [String] -> [[TileType]]
parseInput = fromRight . parseAll (P.many tileType)
 where
  tileType = emptyP <|> vSplitP <|> hSplitP <|> lr <|> rl
  emptyP = Empty <$ P.char '.'
  vSplitP = VSplit <$ P.char '|'
  hSplitP = HSplit <$ P.char '-'
  lr = DiagLR <$ P.char '\\'
  rl = DiagRL <$ P.char '/'
