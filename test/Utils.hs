module Utils (testSolution, testSolution') where

testSolution :: ([String] -> IO Int) -> Int -> IO Int
testSolution solver day = readFile ("input/sample." <> show day <> ".txt") >>= solver . lines

testSolution' :: ([String] -> IO Int) -> String -> IO Int
testSolution' solver day = readFile ("input/sample." <> day <> ".txt") >>= solver . lines
