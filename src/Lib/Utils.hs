module Lib.Utils (readInt, juxt, debug, debug') where
import Debug.Trace (traceShow)

readInt :: String -> Int
readInt = read

juxt :: (Applicative f) => f (a -> b) -> a -> f b
juxt fns = (fns <*>) . pure

debug :: Show b => b -> b
debug x = traceShow x x

debug' :: Show b => String -> b -> b
debug' label x = traceShow (label <> ": " <> show x) x
