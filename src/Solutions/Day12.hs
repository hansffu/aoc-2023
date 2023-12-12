module Solutions.Day12 (day12, test) where

import Data.List.Split (splitOn)

import Lib.Solution (Part, Solution (Solution))
import Lib.TaskRunner (InputType (..), run)
import Lib.Utils (applyT2)

day12 = Solution 12 part1 part2

test = run part1 $ Sample 12

part1 :: Part Int
part1 input' = return $ sum $ length <$> valid
 where
  input = parseInput input'
  templates = fst <$> input
  perms = generatePermutations . length . filter (== '?') . fst <$> input
  possibilities = zipWith applyPermutations templates perms
  valid = zipWith (\ps ns -> filter (`isValidPerm` ns) ps) possibilities (snd <$> input)

part2 :: Part Int
part2 = return . const 0

parseInput :: [String] -> [(String, [Int])]
parseInput = (applyT2 (head, (read <$>) . splitOn "," . last) <$>) . (words <$>)

applyPermutations :: String -> [String] -> [String]
applyPermutations s = (applyPerms s <$>)

applyPerms :: String -> String -> String
applyPerms ('?' : _) [] = error "missing perms"
applyPerms [] _ = ""
applyPerms (c : cs) [] = c : applyPerms cs []
applyPerms ('?' : cs) (p : ps) = p : applyPerms cs ps
applyPerms (c : cs) ps = c : applyPerms cs ps

isValidPerm :: String -> [Int] -> Bool
isValidPerm [] [] = True
isValidPerm [] (_ : _) = False
isValidPerm ('#' : _) [] = False
isValidPerm ('#' : ps) (n : ns)
  | length nextNPs == n = isValidPerm (drop (n - 1) ps) ns
  | length nextNPs > n = False
  | otherwise = False
 where
  nextNPs = '#' : takeWhile (== '#') ps
isValidPerm (_ : ps) ns = isValidPerm (dropWhile (== '.') ps) ns

generatePermutations :: Int -> [[Char]]
generatePermutations 0 = []
generatePermutations 1 = ["#", "."]
generatePermutations len = [a : b | a <- "#.", b <- perms]
 where
  perms = generatePermutations (len - 1)
