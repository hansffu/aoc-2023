module Solutions.Day7 where

import Data.Char (digitToInt)
import Data.List (nub, sort, sortBy)
import Data.Tuple.Extra (both)
import Lib.Solution (Solution (..))
import Lib.TaskRunner (InputType (..), run)

day7 :: Solution Int Int
day7 = Solution 7 part1 part2

test :: IO Int
test = run part1 $ Sample 7

part1 :: [String] -> IO Int
part1 input = do
  let rounds = parseRound <$> input
  return $ sum $ zipWith (*) (snd <$> sort rounds) [1 ..]

part2 :: [String] -> IO Int
part2 input = return 0

newtype Hand = Hand {getCards :: [Int]} deriving (Eq)

instance Ord Hand where
  compare handA handB = if eqType == EQ then compare (getCards handA) (getCards handB) else eqType
   where
    (a, b) = both ((fst <$>) . sortByOccurences . getCards) (handA, handB)
    eqType = compare a b

type Round = (Hand, Int)

parseRound :: String -> Round
parseRound input = (Hand cards, bet)
 where
  bet = read $ last $ words input
  cards = toInt <$> head (words input)
  toInt 'A' = 14
  toInt 'K' = 13
  toInt 'Q' = 12
  toInt 'J' = 11
  toInt 'T' = 10
  toInt c = digitToInt c

sortByOccurences :: (Eq a) => [a] -> [(Int, a)]
sortByOccurences xs = nub $ sortBy (\(a, _) (b, _) -> compare b a) $ zip (occursTimes xs <$> xs) xs

occursTimes :: (Eq a) => [a] -> a -> Int
occursTimes xs x = length $ filter (== x) xs

instance Show Hand where
  show = show . getCards
