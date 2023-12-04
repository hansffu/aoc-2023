module Solutions.Day4 (day4) where

import Data.Either.Utils (fromRight)
import Data.List (intersect)
import Lib.Parser (Parser, intP, parseAll)
import Lib.Solution (Solution (..))
import Lib.TaskRunner (InputType (..), run)
import qualified Text.Parsec as P

day4 :: Solution Int Int
day4 =
  Solution
    { day = 4
    , part1Solution = part1
    , part2Solution = const $ return 0
    }

test = run part1 $ Input 4

part1 :: [String] -> IO Int
part1 input = return $ sum $ fmap (calcScore . length . (\(Card _ w n) -> w `intersect` n)) cards
 where
  cards = fromRight $ parseAll cardP input

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
