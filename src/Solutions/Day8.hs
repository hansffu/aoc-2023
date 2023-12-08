{-# LANGUAGE LambdaCase #-}

module Solutions.Day8 (day8) where

import Data.Either.Utils (fromRight)
import qualified Data.HashMap as Map

import Data.List.Utils (endswith)
import Data.Maybe (fromJust)
import Lib.Parser (Parser, parse)
import Lib.Solution (Solution (Solution))
import qualified Text.Parsec as P

day8 :: Solution Int Int
day8 = Solution 8 part1 part2

part1 :: [String] -> IO Int
part1 input = do
  let (directions, crossroads') = parseMap input
  let crossroads = Map.fromList crossroads'
  return $ length $ takeWhile (/= "ZZZ") $ getSteps crossroads "AAA" (cycle directions)

part2 :: [String] -> IO Int
part2 input = return $ foldl1 lcm $ fst . head . filter (endswith "Z" . snd) . zip [0 ..] <$> steps
 where
  (directions, crossroads') = parseMap input
  crossroads = Map.fromList crossroads'
  allSteps startingPosition = getSteps crossroads startingPosition (cycle directions)
  steps = allSteps <$> filter (endswith "A") (Map.keys crossroads)

getSteps :: Map.Map String (String, String) -> String -> [(String, String) -> String] -> [String]
getSteps crossroads = scanl getNextStep
 where
  getNextStep :: String -> ((String, String) -> String) -> String
  getNextStep currentStep dir = dir $ fromJust $ Map.lookup currentStep crossroads

parseMap :: [String] -> ([(String, String) -> String], [(String, (String, String))])
parseMap input = (directions (head input), crossroads)
 where
  directions = map (\case 'L' -> fst; 'R' -> snd; _ -> error "invalid input")
  crossroads = map (fromRight . parse crossroadP) $ drop 2 input

crossroadP :: Parser (String, (String, String))
crossroadP = do
  key <- P.count 3 P.anyChar <* P.string " = "
  l <- P.char '(' *> P.count 3 P.anyChar <* P.string ", "
  r <- P.count 3 P.anyChar <* P.char ')'
  return (key, (l, r))
