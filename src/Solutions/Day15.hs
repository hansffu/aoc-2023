module Solutions.Day15 where

import Data.List.Split (splitOn)
import Lib.Solution (Part, Solution (Solution), todo)
import Lib.TaskRunner (InputType (..), run)

day15 :: Solution Int Int
day15 = Solution 15 part1 todo

test :: IO Int
test = run part1 $ Input 15

part1 :: Part Int
part1 input = return $ sum $ hash <$> splitOn "," (head input)

hash :: String -> Int
hash = foldl (\acc cur -> ((fromEnum cur + acc) * 17) `mod` 256) 0
