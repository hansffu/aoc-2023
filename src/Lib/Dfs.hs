module Lib.Dfs (dfs) where

import Control.Monad.State (MonadState (state), State, execState)
import Data.Set (Set, empty, insert, member, toList)

data Dfs a = Dfs {visited :: Set a, stack :: [a]}
type DfsState a = State (Dfs a) ()

isVisited :: (Ord a) => a -> State (Dfs a) Bool
isVisited node = state isVisited'
 where
  isVisited' s = (node `member` visited s, s)

visit :: (Ord a) => a -> State (Dfs a) ()
visit node = state visit'
 where
  visit' s = ((), s{visited = insert node (visited s)})

pop :: State (Dfs a) (Maybe a)
pop = state pop'
 where
  pop' s = let (popped, rest) = doPop (stack s) in (popped, s{stack = rest})
  doPop [] = (Nothing, [])
  doPop (x : xs) = (Just x, xs)

pushAll :: [a] -> State (Dfs a) ()
pushAll xs = state (\s -> ((), s{stack = xs <> stack s}))

dfs :: (Ord a) => (a -> [a]) -> a -> [a]
dfs getNeighbours firstNode = toList $ visited $ execState (doDfs getNeighbours) (Dfs empty [firstNode])

doDfs :: (Ord a) => (a -> [a]) -> DfsState a
doDfs getNeighbours = do
  next <- pop
  case next of
    Nothing -> return ()
    Just node -> do
      visited' <- isVisited node
      if visited'
        then doDfs getNeighbours
        else do
          visit node
          let neighbours = getNeighbours node
          pushAll neighbours
          doDfs getNeighbours
