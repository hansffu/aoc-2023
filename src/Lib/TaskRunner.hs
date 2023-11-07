-- | yo
module Lib.TaskRunner (solve) where

getInput :: String -> IO [String]
getInput filename = lines <$> readFile filename

data InputType = Input Int | Sample Int

solve :: InputType -> ([String] -> IO String) -> IO String
solve inputType solver = do
    input <- getInput $ getFilename inputType
    result <- solver input
    return $ "Result: " <> result

getFilename :: InputType -> String
getFilename (Input n) = "input." <> show n <> ".txt"
getFilename (Sample n) = "sample." <> show n <> ".txt"
