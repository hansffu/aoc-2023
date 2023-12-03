module Lib.Stack where

import Control.Monad.State (State, state)

type Stack a = [a]

pop :: State (Stack a) (Maybe a)
pop = state doPop
 where
  doPop :: [a] -> (Maybe a, [a])
  doPop [] = (Nothing, [])
  doPop (x : xs) = (Just x, xs)

popIf :: (a -> Bool) -> State (Stack a) (Maybe a)
popIf test = state doPop
 where
  doPop [] = (Nothing, [])
  doPop (x : xs)
    | test x = (Just x, xs)
    | otherwise = (Nothing, x : xs)
