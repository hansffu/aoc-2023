{-# LANGUAGE PatternGuards #-}

module Solutions.Day16 (day16, test) where

import Control.Applicative ((<|>))
import Control.Monad.State (MonadState (get), State, execState, runState, state)
import Data.Either.Utils (fromRight)

import Data.List (nub)
import Data.Set (Set, empty, insert, member, notMember, toList)
import Lib.Parser (parse, parseAll)
import Lib.Solution (Part, Solution (..), todo)
import Lib.TaskRunner (InputType (..), run)
import Lib.Utils (debug, debug', prettyPrint)
import qualified Text.Parsec as P

day16 :: Solution Int Int
day16 = Solution 16 todo todo

test :: IO Int
test = run part1 $ Input 16

part1 :: Part Int
part1 input = do
  -- prettyPrint input
  -- print $ nextSteps tiles $ Step (0, 0) E
  let a = toList $ visited $ execState (goLights tiles) (Dfs empty [Step (0, 0) E])
  let b = nub $ coord <$> a
  -- print $ length b
  -- print $ take 10 $ goLights empty tiles (Step (0, 0) E)
  return $ length $ nub $ coord <$> a
 where
  tiles = parseInput input

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
          -- let neighbours = debug' (show (coord step) <> show (direction step) <> ": ") $ nextSteps tiles step
          let neighbours = nextSteps tiles step
          pushAll neighbours
          goLights tiles

-- goLights :: Set Step -> [[TileType]] -> Step -> [Step]
-- goLights visited tiles current = insertAll visited'
--  where
--   neighbours = filter (`notMember` visited) $ nextSteps tiles current
--   visited' = insert current visited
--   nn = neighbours >>= goLights visited' tiles
--   insertAll :: Set a -> [a] -> Set a
--   insertAll s [] = s
--   insertAll s (v : vs) = insertAll (insert v s) vs

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

parseInput = fromRight . (parseAll $ P.many tileType)
 where
  tileType = emptyP <|> vSplitP <|> hSplitP <|> lr <|> rl
  emptyP = Empty <$ P.char '.'
  vSplitP = VSplit <$ P.char '|'
  hSplitP = HSplit <$ P.char '-'
  lr = DiagLR <$ P.char '\\'
  rl = DiagRL <$ P.char '/'
