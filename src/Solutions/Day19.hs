module Solutions.Day19 (day19, test) where

import Data.List.Split (splitOn)

import Data.List.Extra (firstJust, unsnoc)
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe)
import Lib.Parser (Parser, intP, parseAll')
import Lib.Solution (Part, Solution (..), todo)
import Lib.TaskRunner (InputType (Sample), run)
import Lib.Utils (applyT2)
import qualified Text.Parsec as P

day19 :: Solution Int Int
day19 = Solution 19 part1 todo

test :: IO Int
test = run part1 $ Sample 19

data Condition = Condition {var :: Char, comparison :: Char, n :: Int, dest :: String} deriving (Show)
data Tool = Tool {x :: Int, m :: Int, a :: Int, s :: Int} deriving (Show)
type ProcedureSpec = (String, [Condition], String)

part1 :: Part Int
part1 input = do
  let acceptedTools = filter (isAccepted procedures "in") tools
  return $ sum $ sumTool <$> acceptedTools
 where
  (procedureSpecs, tools) = parseInput input
  procedures = M.fromList $ buildProcedure <$> procedureSpecs
  sumTool tool = x tool + m tool + a tool + s tool

isAccepted :: M.Map String (Tool -> String) -> String -> Tool -> Bool
isAccepted procedures pid tool = case (procedures M.! pid) tool of
  "A" -> True
  "R" -> False
  nextPid -> isAccepted procedures nextPid tool

buildProcedure :: ProcedureSpec -> (String, Tool -> String)
buildProcedure ps@(pid, _, _) = (pid, buildProcedure' ps)

buildProcedure' :: ProcedureSpec -> Tool -> String
buildProcedure' (_, conditions', default') tool' = fromMaybe default' $ firstJust id conditions
 where
  conditions = buildCondition tool' <$> conditions'
  buildCondition tool condition = if varValue tool `comparison'` n condition then Just (dest condition) else Nothing
   where
    varValue = case var condition of
      'x' -> x
      'm' -> m
      'a' -> a
      's' -> s
      c -> error $ c : " is not a valid var"
    comparison' :: Int -> Int -> Bool
    comparison' = case comparison condition of
      '<' -> (<)
      '>' -> (>)
      c -> error $ c : "is not a valid condition"

parseInput :: [String] -> ([ProcedureSpec], [Tool])
parseInput input = (parseAll' procedureP procedures, parseAll' toolP tools)
 where
  (procedures, tools) = applyT2 (head, last) $ splitOn [""] input
  procedureP :: Parser (String, [Condition], String)
  procedureP = do
    pid <- P.many P.letter
    checks <- P.char '{' *> P.many (P.noneOf "}") <* P.char '}'
    let (conditions', default') = fromJust $ unsnoc $ splitOn "," checks
    let conditions = parseAll' checkP conditions'
    return (pid, conditions, default')

  checkP = do
    var' <- P.anyChar
    comparison' <- P.try $ P.oneOf "<>"
    n' <- intP <* P.char ':'
    dest' <- P.many P.alphaNum
    return $ Condition var' comparison' n' dest'

  toolP = do
    x' <- P.string "{x=" *> intP <* P.char ','
    m' <- P.string "m=" *> intP <* P.char ','
    a' <- P.string "a=" *> intP <* P.char ','
    s' <- P.string "s=" *> intP <* P.char '}'
    return $ Tool x' m' a' s'
