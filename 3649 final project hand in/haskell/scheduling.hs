module Scheduling where

import Types (Activity(..))
import Helpers (addMinutes)
import Data.List (find, sortBy, sortOn)
import Data.Maybe (fromJust)



-- Generates all possible start times for an activity
generateStartTimes :: Activity -> [Int]
generateStartTimes activity =
  generateStartTimesHelper (startRange activity) (endRange activity) (duration activity)
  where
    generateStartTimesHelper start end duration
      | addMinutes start duration <= end = start : generateStartTimesHelper (addMinutes start 1) end duration
      | otherwise = []

-- Chooses a value from the start time options list, based on if there is a conflict with a previous activity
chooseStartTime :: [Int] -> [Activity] -> [Activity] -> Int -> Maybe Int
chooseStartTime [] _ _ _ = Nothing
chooseStartTime (startTime:xs) previousActivities remainingActivities duration =
  let endTime = addMinutes startTime duration
      ifConflictFound = conflictsWith startTime endTime (previousActivities ++ remainingActivities)
  in if ifConflictFound then chooseStartTime xs previousActivities remainingActivities duration else Just startTime



conflictsWith :: Int -> Int -> [Activity] -> Bool
conflictsWith start end activities =
  let conflictingActivities = filter (conflictFound start end) activities
  in not $ null conflictingActivities

-- Check if two activities have a conflict
conflictFound :: Int -> Int -> Activity -> Bool
conflictFound start1 end1 activity2 =
  let start2 = actualStart activity2
      end2 = actualEnd activity2
  in start1 < end2 && end1 > start2


schedule :: [Activity] -> [Activity]
schedule activities = head $ scheduleActivities activities []


--use list comprehension here
scheduleActivities :: [Activity] -> [Activity] -> [[Activity]]
scheduleActivities [] scheduledActivities = [scheduledActivities]
scheduleActivities (currentActivity:remainingActivities) scheduledActivities =
  [ scheduledActivity : restOfSchedule
  | startTime <- generateStartTimes currentActivity
  , not $ conflictsWith startTime (addMinutes startTime (duration currentActivity)) scheduledActivities
  , let scheduledActivity = currentActivity { actualStart = startTime, actualEnd = addMinutes startTime (duration currentActivity) }
  , restOfSchedule <- scheduleActivities remainingActivities (scheduledActivities ++ [scheduledActivity])
  ]