module Solutions.Day16 (day16, test) where

import Control.Applicative ((<|>))
import Control.Monad.State (State, execState, state)
import Data.Either.Utils (fromRight)
import Data.List (nub)
import Data.Set (Set, empty, insert, member, toList)
import Lib.Parser (parseAll)
import Lib.Solution (Part, Solution (..), todo)
import Lib.TaskRunner (InputType (..), run)
import qualified Text.Parsec as P

day16 :: Solution Int Int
day16 = Solution 16 part1 todo

test :: IO Int
test = run part1 $ Sample 16

part1 :: Part Int
part1 input = return $ length $ nub $ coord <$> visitedState
 where
  tiles = parseInput input
  visitedState = toList $ visited $ execState (goLights tiles) (Dfs empty [Step (0, 0) E])

data Dfs = Dfs {visited :: Set Step, stack :: [Step]}
type DfsState = State Dfs ()

isVisited :: Step -> State Dfs Bool
isVisited step = state isVisited'
 where
  isVisited' s = (step `member` visited s, s)

visit :: Step -> State Dfs ()
visit step = state visit'
 where
  visit' s = ((), s{visited = insert step (visited s)})

pop :: State Dfs (Maybe Step)
pop = state pop'
 where
  pop' s = let (popped, rest) = doPop (stack s) in (popped, s{stack = rest})
  doPop [] = (Nothing, [])
  doPop (x : xs) = (Just x, xs)

pushAll :: [Step] -> State Dfs ()
pushAll xs = state (\s -> ((), s{stack = xs <> stack s}))

goLights :: [[TileType]] -> DfsState
goLights tiles = do
  next <- pop
  case next of
    Nothing -> return ()
    Just step -> do
      visited' <- isVisited step
      if visited'
        then goLights tiles
        else do
          visit step
          let neighbours = nextSteps tiles step
          pushAll neighbours
          goLights tiles

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
