module Main where

import Types (Activity(..))
import Helpers (sortByActualEndTime)
import IO (printActivities)

main :: IO ()
main = do
  let a1 = Activity "A" 900 1000 900 True 0 10
      a2 = Activity "B" 1000 1100 1000 True 0 5
      a3 = Activity "C" 1100 1200 600 True 0 2
      a4 = Activity "D" 1000 1100 800 True 0 50
      a5 = Activity "E" 900 1000 700 True 0 75
      activities = [a1, a2, a3, a4, a5]
      sortedActivities = sortByActualEndTime activities
  printActivities sortedActivities