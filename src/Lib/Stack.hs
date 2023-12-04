module Lib.Stack where

import Control.Monad.State (State, state)

type Stack a = [a]

pop :: State (Stack a) (Maybe a)
pop = state doPop
 where
  doPop :: [a] -> (Maybe a, [a])
  doPop [] = (Nothing, [])
  doPop (x : xs) = (Just x, xs)

pop' :: State (Stack a) a
pop' = state doPop
 where
  doPop :: [a] -> (a, [a])
  doPop [] = error "empty stack"
  doPop (x : xs) = (x, xs)

push :: a -> State (Stack a) ()
push x = state (\xs -> ((), x : xs))

pushAll :: [a] -> State (Stack a) ()
pushAll xs = state $ \xs' -> ((), xs <> xs')

isEmpty :: State (Stack a) Bool
isEmpty = state isEmpty'
 where
  isEmpty' [] = (True, [])
  isEmpty' xs = (False, xs)

popIf :: (a -> Bool) -> State (Stack a) (Maybe a)
popIf test = state doPop
 where
  doPop [] = (Nothing, [])
  doPop (x : xs)
    | test x = (Just x, xs)
    | otherwise = (Nothing, x : xs)
