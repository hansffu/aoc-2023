{-# LANGUAGE LambdaCase #-}

module Solutions.Day8 where

import Data.Either.Utils (fromRight)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Lib.Parser (Parser, parse)
import Lib.Solution (Solution (Solution))
import Lib.TaskRunner (InputType (..), run)
import qualified Text.Parsec as P

day8 :: Solution Int Int
day8 = Solution 8 part1 part2

test :: IO Int
test = run part1 $ Sample 8

part1 :: [String] -> IO Int
part1 input = do
  let (directions, crossroads') = parseMap input
  let crossroads = Map.fromList crossroads'
  let res = countSteps crossroads (cycle directions) "AAA"
  return res

countSteps :: Map.Map String (String, String) -> [(String, String) -> String] -> String -> Int
countSteps _ [] _ = error "no more steps"
countSteps crossroads (dir : dirs) currentStep
  | currentStep == "ZZZ" = 0
  | otherwise = 1 + countSteps crossroads dirs nexStep
 where
  nexStep = dir $ fromJust $ Map.lookup currentStep crossroads

part2 :: [String] -> IO Int
part2 input = return 0

-- parseMap :: [string] -> ([String -> String], (String, String))
parseMap :: [String] -> ([(String, String) -> String], [(String, (String, String))])
parseMap input = (directions (head input), crossroads)
 where
  directions = map (\case 'L' -> fst; 'R' -> snd; _ -> error "invalid input")
  crossroads = map (fromRight . parse crossroadP) $ drop 2 input

crossroadP :: Parser (String, (String, String))
crossroadP = do
  key <- P.many P.letter <* P.string " = "
  l <- P.char '(' *> P.count 3 P.anyChar <* P.string ", "
  r <- P.count 3 P.anyChar <* P.char ')'
  return (key, (l, r))

