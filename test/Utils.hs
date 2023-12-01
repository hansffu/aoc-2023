module Utils (testSolution) where

testSolution :: ([String] -> IO Int) -> Int -> IO Int
testSolution solver day = readFile ("input/sample." <> show day <> ".txt") >>= solver . lines
