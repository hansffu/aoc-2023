{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Solutions.Day14 (day14, test) where

import Data.List (sortBy, transpose)
import Data.Ord (comparing)
import qualified Data.Ord
import Lib.Solution (Part, Solution (Solution), todo)
import Lib.TaskRunner (InputType (..), run)

test = run part1 $ Sample 14

day14 = Solution 14 part1 todo

part1 :: Part Int
part1 input = return $ sum $ fst <$> filter ((== 'O') . snd) (afterFalling >>= (zip [1 ..] . reverse))
 where
  afterFalling = fallLeft <$> transpose input

fallLeft :: String -> String
fallLeft [] = []
fallLeft ('#' : xs) = '#' : fallLeft xs
fallLeft s = loose ++ fallLeft rest
 where
  loose = sortBy (comparing Data.Ord.Down) $ takeWhile (/= '#') s
  rest = dropWhile (/= '#') s
