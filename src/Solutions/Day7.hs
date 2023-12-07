module Solutions.Day7 where

import Data.Char (digitToInt)
import Data.List (nub, sort, sortBy)
import Data.List.Extra (replace)
import Data.Tuple.Extra (both)
import Lib.Solution (Solution (..))

day7 :: Solution Int Int
day7 = Solution 7 part1 part2

part1 :: [String] -> IO Int
part1 input = do
  let rounds = parseRound <$> input
  return $ sum $ zipWith (*) (snd <$> sort rounds) [1 ..]

part2 :: [String] -> IO Int
part2 input = do
  let rounds = transformRound . parseRound <$> input
  return $ sum $ zipWith (*) (snd <$> sort rounds) [1 ..]

transformRound :: Round -> Round2
transformRound (Hand cards, bet) = ((\h -> JokerHand h (maximum $ permutations h)) (Hand (jTo0 <$> cards)), bet)
 where
  jTo0 = jToX 0
  jToX x 11 = x
  jToX _ c = c

permutations :: Hand -> [Hand]
permutations (Hand cards) =
  [ Hand $ replace [0] [x] cards
  | x <- [0 .. 14]
  , x /= 11
  ]

newtype Hand = Hand {getCards :: [Int]} deriving (Eq)
data JokerHand = JokerHand {originals :: Hand, improved :: Hand} deriving (Eq)

compareType :: Hand -> Hand -> Ordering
compareType handA handB = compare a b
 where
  (a, b) = both ((fst <$>) . sortByOccurences . getCards) (handA, handB)

instance Ord Hand where
  compare handA handB = if eqType == EQ then compare (getCards handA) (getCards handB) else eqType
   where
    eqType = compareType handA handB

instance Ord JokerHand where
  compare handA handB = if eqType == EQ then compare (getCards $ originals handA) (getCards $ originals handB) else eqType
   where
    eqType = compareType (improved handA) (improved handB)

type Round = (Hand, Int)
type Round2 = (JokerHand, Int)

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
