module Solutions.Day4 (day4) where

import Data.Array (listArray, (!))
import Data.Either.Utils (fromRight)
import Data.List (intersect)
import Lib.Parser (Parser, intP, parseAll)
import Lib.Solution (Solution (..))

import qualified Text.Parsec as P

day4 :: Solution Int Int
day4 =
  Solution
    { day = 4
    , part1Solution = part1
    , part2Solution = part2
    }

part1 :: [String] -> IO Int
part1 input = return $ sum (calcScore . length . (\(Card _ w n) -> w `intersect` n) <$> cards input)

part2 :: [String] -> IO Int
part2 input = do
  return $ sum nums
 where
  cardList = (\(Card cid' w' n') -> Card (cid' - 1) w' n') <$> cards input
  arrLen = (0, length cardList - 1)
  cardArray = listArray (0, length cardList - 1) cardList
  scores = length . (\(Card _ w n) -> w `intersect` n) <$> cardArray
  pointsTo =
    listArray arrLen $
      [ [currentCard + offset | offset <- [0 .. (scores ! currentCard)], offset /= 0]
      | currentCard <- [0 .. (length scores - 1)]
      ]
  pointedToBy = listArray arrLen $ fmap (\x -> filter (\y -> x `elem` (pointsTo ! y)) [0 .. x]) [0 .. length cardList - 1]
  nums = 1 : next 1
   where
    next i
      | i <= snd arrLen = 1 + sum ((nums !!) <$> (pointedToBy ! i)) : next (i + 1)
      | otherwise = []

cards :: [String] -> [Card]
cards = fromRight . parseAll cardP

calcScore :: Int -> Int
calcScore 0 = 0
calcScore 1 = 1
calcScore n = 2 * calcScore (n - 1)

data Card = Card
  { cid :: Int
  , winners :: [Int]
  , numbers :: [Int]
  }
  deriving (Show)

cardP :: Parser Card
cardP = do
  cid' <- P.string "Card" *> P.spaces *> intP <* P.char ':' <* P.spaces
  w <- P.many intP <* P.spaces <* P.char '|' <* P.spaces
  n <- P.many intP
  return $ Card cid' w n
