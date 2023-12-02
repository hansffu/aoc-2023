module Solutions.Day2 where

import Data.Char (isDigit)
import Data.Foldable (find)
import Data.List.Split (splitOn)
import Data.List.Utils (contains)

import Data.String.Utils (strip)

import Data.Tuple.Utils (fst3, snd3)
import Lib.Solution (Solution (Solution))
import Lib.Utils (readInt)

day2 :: Solution Int Int
day2 = Solution 2 part1 part2

part1 :: [String] -> IO Int
part1 = return . sum . map gameId . filter isValidGame . map parseGame
 where
  isValidDraw (r, g, b) = r <= 12 && g <= 13 && b <= 14
  isValidGame game = all isValidDraw $ draws game

part2 :: [String] -> IO Int
part2 = return . sum . map (getPower . parseGame)
 where
  getPower game =
    let
      red = maximum $ map fst3 $ draws game
      green = maximum $ map snd3 $ draws game
      blue = maximum $ map (\(_, _, b) -> b) $ draws game
     in
      red * green * blue

data Game = Game {gameId :: Int, draws :: [Draw]} deriving (Show)
type Draw = (Int, Int, Int)

parseGame :: String -> Game
parseGame input = Game gameId' $ map parseDraw drawTexts
 where
  parts = splitOn ":" input
  gameText = head parts
  drawsText = parts !! 1
  gameId' = read $ filter isDigit gameText
  drawTexts = map strip $ splitOn ";" drawsText

parseDraw :: String -> Draw
parseDraw input = (red, green, blue)
 where
  colors = map strip $ splitOn "," input
  getColor color = maybe 0 (readInt . head . splitOn " ") (find (contains color) colors)
  red = getColor "red"
  green = getColor "green"
  blue = getColor "blue"
