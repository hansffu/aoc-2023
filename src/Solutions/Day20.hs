{-# LANGUAGE TupleSections #-}

module Solutions.Day20 (day20, test) where

import qualified Data.Map as M
import Lib.Parser (parseAll')
import Lib.Solution (Part, Solution (..), todo)
import Lib.TaskRunner (InputType (..), run)

import Control.Applicative ((<|>))

import Data.Maybe (listToMaybe)

import Lib.Utils (applyT2)
import qualified Text.Parsec as P

day20 :: Solution Int Int
day20 = Solution 20 part1 todo

test :: IO Int
test = run part1 $ Sample 20

type Destination = String
type Source = String
data SignalType = High | Low deriving (Show, Eq)
data Signal = Signal SignalType Source Destination deriving (Show)
data Module
  = Broadcaster [Destination]
  | FlipFlop SignalType [Destination]
  | Conjunction (M.Map Source SignalType) [Destination]
  deriving (Show)

data SignalState = SignalState
  { queue :: [Signal]
  , modules :: M.Map String Module
  , highSignals :: Int
  , lowSignals :: Int
  }
  deriving (Show)

type ModuleMap = M.Map String Module

part1 :: Part Int
part1 input = do
  return $ h * l
 where
  buttonPresses = 1000
  parsed = updateConjunctionWithSource $ M.fromList $ parseInput input
  startingState = SignalState{queue = [], modules = parsed, highSignals = 0, lowSignals = 0}
  (h, l) = countSignals $ pushButtonTimes buttonPresses startingState

countSignals :: SignalState -> (Int, Int)
countSignals = applyT2 (highSignals, lowSignals)

updateConjunctionWithSource :: M.Map String Module -> M.Map String Module
updateConjunctionWithSource modules' = M.mapWithKey update modules'
 where
  update key (Conjunction _ dests) = Conjunction (M.fromList (getMatching key)) dests
  update _ module' = module'
  getMatching key = (\(src, _) -> (src, Low)) <$> filter (\(_, dest) -> dest == key) mappings
  mappings = M.toList modules' >>= sourceMapping
  sourceMapping (src, m) = case m of
    Broadcaster dests -> (src,) <$> dests
    FlipFlop _ dests -> (src,) <$> dests
    Conjunction _ dests -> (src,) <$> dests

pushButtonTimes :: Int -> SignalState -> SignalState
pushButtonTimes 0 s = s
pushButtonTimes n s = pushButtonTimes (n - 1) (pushButton s)

pushButton :: SignalState -> SignalState
pushButton s = processSignals s{queue = [Signal Low "init" "broadcaster"], lowSignals = lowSignals s + 1}

processSignals :: SignalState -> SignalState
processSignals s@(SignalState{queue = []}) = s
processSignals s@(SignalState{queue = (next : xs)}) = processSignals nextState
 where
  (producedSignals, updatedModules) = processSignal next (modules s)
  (h, l) = case listToMaybe producedSignals of
    Nothing -> (0, 0)
    Just (Signal High _ _) -> (length producedSignals, 0)
    Just (Signal Low _ _) -> (0, length producedSignals)
  nextState =
    SignalState
      { queue = xs ++ producedSignals
      , modules = updatedModules
      , highSignals = highSignals s + h
      , lowSignals = lowSignals s + l
      }

processSignal :: Signal -> ModuleMap -> ([Signal], ModuleMap)
processSignal signal@(Signal _ _ dest) modules' = (producedSignals, insert' updatedModule)
 where
  insert' :: Maybe Module -> ModuleMap
  insert' Nothing = modules'
  insert' (Just m) = M.insert dest m modules'
  updatedModule = updateModuleState modules' signal dest
  producedSignals = maybe [] (newSignals signal) updatedModule
  newSignals (Signal signal' _ _) (Broadcaster destinations) = Signal signal' dest <$> destinations
  newSignals (Signal High _ _) (FlipFlop _ _) = []
  newSignals (Signal Low _ _) (FlipFlop signalType destinations) = Signal signalType dest <$> destinations
  newSignals (Signal{}) (Conjunction sources' destinations)
    | all ((== High) . snd) (M.toList sources') = Signal Low dest <$> destinations
    | otherwise = Signal High dest <$> destinations

updateModuleState :: ModuleMap -> Signal -> Destination -> Maybe Module
updateModuleState modules' signal dest = getUpdatedModuleState signal <$> M.lookup dest modules'
 where
  getUpdatedModuleState :: Signal -> Module -> Module
  getUpdatedModuleState _ b@(Broadcaster{}) = b
  getUpdatedModuleState (Signal High _ _) flipFlop@(FlipFlop{}) = flipFlop
  getUpdatedModuleState (Signal Low _ _) (FlipFlop currentType dest') = FlipFlop (flipSignalType currentType) dest'
  getUpdatedModuleState (Signal st source _) (Conjunction sources dest') = Conjunction (M.insert source st sources) dest'

flipSignalType :: SignalType -> SignalType
flipSignalType High = Low
flipSignalType Low = High

parseInput :: [String] -> [(String, Module)]
parseInput = parseAll' moduleP
 where
  moduleP = broadcasterP <|> flipFlopP <|> conjunctionP
  broadcasterP = do
    dests <- P.try (P.string "broadcaster") *> destinationsP
    return ("broadcaster", Broadcaster dests)
  flipFlopP = do
    id' <- P.try (P.char '%') *> P.many1 P.letter
    dest <- destinationsP
    return (id', FlipFlop Low dest)
  conjunctionP = do
    id' <- P.try (P.char '&') *> P.many1 P.letter
    dest <- destinationsP
    return (id', Conjunction M.empty dest)
  destinationsP = P.string " -> " *> P.sepBy1 (P.many1 P.letter) (P.string ", ")
