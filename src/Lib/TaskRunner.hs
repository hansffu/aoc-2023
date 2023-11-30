-- | yo
module Lib.TaskRunner (run, readInput, InputType (..)) where

readInput :: InputType -> IO [String]
readInput inputType = lines <$> readFile (getFilename inputType)

data InputType = Input Int | Sample Int

type Task a = [String] -> IO a

run :: Task a -> InputType -> IO a
run solver inputType = do
  input <- readInput inputType
  solver input


getFilename :: InputType -> String
getFilename (Input n) = "input/input." <> show n <> ".txt"
getFilename (Sample n) = "input/sample." <> show n <> ".txt"
