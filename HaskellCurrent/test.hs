module Main where

import Types (Activity(..))
import Helpers (sortByActualEndTime)
import MyIO (printActivities)
import Scheduling (schedule, splitInHalf)

main :: IO ()
main = do
  let a1 = Activity "Gym" 830 1130 120 True 0 0
      a2 = Activity "Lunch" 1200 1300 30 True 0 0
      a3 = Activity "Homework" 1200 1630 120 True 0 0
      a4 = Activity "Breakfast" 830 930 30 True 0 0
      a5 = Activity "Dinner" 1800 2000 30 True 0 0
      activities = [a1, a2, a3, a4, a5]
      scheduledActivities = schedule activities
      finalActivities = splitInHalf scheduledActivities
      sortedActivities = sortByActualEndTime finalActivities
  printActivities sortedActivities