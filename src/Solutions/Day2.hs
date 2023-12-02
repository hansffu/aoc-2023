module Solutions.Day2 where

import Data.Foldable (find)
import Text.Parsec ((<|>))
import qualified Text.Parsec as P

import Data.Either.Utils (fromRight)
import Data.Tuple.Utils (fst3, snd3)
import Lib.Parser (Parser, intP, parseAll)
import Lib.Solution (Solution (Solution))

day2 :: Solution Int Int
day2 = Solution 2 part1 part2

part1 :: [String] -> IO Int
part1 = return . sum . map gameId . filter isValidGame . parseGame
 where
  isValidDraw (r, g, b) = r <= 12 && g <= 13 && b <= 14
  isValidGame game = all isValidDraw $ draws game

part2 :: [String] -> IO Int
part2 = return . sum . map getPower . parseGame
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

parseGame :: [String] -> [Game]
parseGame = fromRight . parseAll gameP

colorP :: Parser (String, Int)
colorP = do
  num <- intP <* P.spaces
  color <- P.string "red" <|> P.string "green" <|> P.string "blue"
  return (color, num)

drawP :: Parser Draw
drawP = do
  colors <- P.sepBy colorP (P.char ',' <* P.spaces)
  let findColor color = maybe 0 snd (find ((== color) . fst) colors)
  return (findColor "red", findColor "green", findColor "blue")

gameP :: Parser Game
gameP = do
  gid <- P.string "Game" *> P.spaces *> intP <* P.char ':' <* P.spaces
  draws' <- P.sepBy drawP (P.char ';' <* P.spaces)
  return $ Game gid draws'
