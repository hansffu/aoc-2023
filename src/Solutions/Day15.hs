module Solutions.Day15 (day15, test) where

import Data.Either.Utils (fromRight)

import Control.Monad (mfilter)
import Lib.Parser (Parser, parse)
import Lib.Solution (Part, Solution (Solution), todo)
import Lib.TaskRunner (InputType (..), run)

import Data.IntMap (IntMap, adjust, fromList)
import Data.Maybe (fromMaybe)
import Text.Parsec ((<|>))
import qualified Text.Parsec as P

day15 :: Solution Int Int
day15 = Solution 15 part1 part2

test :: IO Int
test = run part2 $ Sample 15

part1 :: Part Int
part1 = return . sum . ((hashInstruction <$>) . parseLenses . head)

part2 :: Part Int
part2 input = return $ sum $ focusingPower <$> processed
 where
  instructions' = parseLenses (head input)
  boxes = fromList $ (\i -> (i, Box i [])) <$> [0 .. 256]
  processed = process boxes  instructions'

focusingPower :: Box -> Int
focusingPower b = sum $ (* boxPower) <$> zipWith (*) [1 ..] (lensPower <$> lenses b)
 where
  boxPower = boxNum b + 1
  lensPower (Instruction (_, _, power)) = fromMaybe 0 power

process :: IntMap Box -> [Instruction] -> IntMap Box
process boxes [] = boxes
process boxes (i@(Instruction (label, _, _)) : is) = process adjusted is
 where
  adjusted = adjust (processBox i) hash' boxes
  hash' = hash label

processBox :: Instruction -> Box -> Box
processBox i@(Instruction (label, Insert, _)) b = Box (boxNum b) (if not (any (sameLabel i) (lenses b)) then addedLens else replacedLens)
 where
  sameLabel (Instruction (label1, _, _)) (Instruction (label2, _, _)) = label1 == label2
  addedLens = lenses b <> pure i
  replacedLens = (\i'@(Instruction (label', _, _)) -> if label == label' then i else i') <$> lenses b
processBox (Instruction (label, Remove, _)) b = Box (boxNum b) newLenses
 where
  newLenses = filter (\(Instruction (label', _, _)) -> label /= label') $ lenses b

data Command = Insert | Remove deriving (Eq)

newtype Instruction = Instruction (String, Command, Maybe Int) deriving (Eq)
type Lens = Instruction

data Box = Box
  { boxNum :: Int
  , lenses :: [Lens]
  }
  deriving (Show)

hashInstruction :: Instruction -> Int
hashInstruction = hash . show

hash :: String -> Int
hash = foldl (\acc cur -> ((fromEnum cur + acc) * 17) `mod` 256) 0

parseLenses :: String -> [Instruction]
parseLenses = fromRight . parse (P.sepBy lensParser (P.char ','))
 where
  lensParser :: Parser Instruction
  lensParser = do
    a <- P.many1 P.letter
    b <- (Insert <$ P.char '=') <|> (Remove <$ P.char '-')
    c <- P.many P.digit
    return $ Instruction (a, b, read <$> mfilter (not . null) (pure c))

instance Show Instruction where
  show (Instruction (a, b, c)) = a <> show b <> maybe "" show c

instance Show Command where
  show Insert = "="
  show Remove = "-"
