--sample run
-- ghc -o inputoutput -main-is InputOutput inputoutput.hs
--./inputoutput normal.txt 

module InputOutput where

import Helpers (sortByActualEndTime, splitInHalf)
import Data.List.Split (splitOn)
import System.Environment (getArgs)
import Data.Char (isDigit)
import Types (Activity(..))
import Control.Exception (tryJust)
import Control.Monad (guard)
import System.IO.Error (isDoesNotExistError, tryIOError)
import Scheduling (schedule)

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
  in Activity name startRange endRange duration True 0 0

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
      let realActivities = schedule activities
      let split = splitInHalf realActivities
      let sortedActivities = sortByActualEndTime split
      printActivities sortedActivities
    _ -> putStrLn "Usage: programname inputfilename"