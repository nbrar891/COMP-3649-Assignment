module MyIO where

import Data.List.Split (splitOn)
import System.Environment (getArgs)
import Data.Char (isDigit)
import Types (Activity(..))


readActivities :: FilePath -> IO [Activity]
readActivities filename = do
  contents <- readFile filename
  return (map parseActivity (lines contents))

parseActivity :: String -> Activity
parseActivity input =
  let fields = splitOn "-" input
      name = fields !! 0
      startRange = read (fields !! 1)
      endRange = read (fields !! 2)
      duration = read (takeWhile isDigit (fields !! 3))
  in Activity name startRange endRange duration False 0 0

printActivity :: Int -> Activity -> IO ()
printActivity index activity =
  putStrLn (show index ++ "." ++ name activity ++ "-" ++ show (actualStart activity) ++ "-" ++ show (actualEnd activity) ++ "-" ++ show (duration activity))

printActivities :: [Activity] -> IO ()
printActivities activities =
  mapM_ (uncurry printActivity) (zip [1..] activities)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      activities <- readActivities filename
      printActivities activities
    _ -> putStrLn "Usage: programname inputfilename"