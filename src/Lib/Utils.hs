module Lib.Utils (readInt, juxt, debug, debug', prettyPrint, index2d, applyT2, Array2d, toArray2d) where

import Data.Array (Array, listArray)
import Data.List.Utils (join)
import Debug.Trace (traceShow)

readInt :: String -> Int
readInt = read

juxt :: (Applicative f) => f (a -> b) -> a -> f b
juxt fns = (fns <*>) . pure

debug :: (Show b) => b -> b
debug x = traceShow x x

debug' :: (Show b) => String -> b -> b
debug' label x = traceShow (label <> ": " <> show x) x

prettyPrint :: (Show a) => [a] -> IO ()
prettyPrint = putStrLn . join "\n" . map show

index2d :: [[a]] -> [[((Int, Int), a)]]
index2d = zipWith (\i -> zipWith (\j x -> ((i, j), x)) [0 ..]) [0 ..]

type Array2d a = Array Int (Array Int a)
toArray2d :: [[a]] -> Array2d a
toArray2d rows = listArray (0, length rows - 1) $ listArray (0, length (head rows) - 1) <$> rows

applyT2 :: (t -> a, t -> b) -> t -> (a, b)
applyT2 (f1, f2) b = (f1 b, f2 b)
