module Scheduling where

import Types (Activity(..))
import Helpers (addMinutes)
import Data.List (find)
-- import Myio (printActivities)


-- Generates all possible start times for an activity
generateStartTimes :: Activity -> [Int]
generateStartTimes activity =
  generateStartTimesHelper (startRange activity) (endRange activity) (duration activity)
  where
    generateStartTimesHelper start end duration
      | addMinutes start duration <= end = start : generateStartTimesHelper (addMinutes start 5) end duration
      | otherwise = []

-- Assigns start times to activities
assignStartTime :: Activity -> Activity
assignStartTime activity@(Activity name startRange endRange duration allowed _ _) =
  let startTimes = generateStartTimes activity
      assignStartTimeHelper :: [Int] -> Activity
      assignStartTimeHelper [] = activity -- No suitable start time found, return original activity
      assignStartTimeHelper (start:rest) =
        let actualEnd = addMinutes start duration
        in if actualEnd <= endRange
            then activity { actualStart = start, actualEnd = actualEnd } -- Found a suitable start time
            else assignStartTimeHelper rest -- Continue with the next start time option
  in assignStartTimeHelper startTimes


-- Check if two activities have a conflict
conflictFound :: Activity -> Activity -> Bool
conflictFound activity1 activity2 =
  let start1 = actualStart activity1
      end1 = actualEnd activity1
      start2 = actualStart activity2
      end2 = actualEnd activity2
  in start1 < end2 && end1 > start2

-- Schedule an activity by assigning start time and end time
scheduleActivity :: Activity -> [Activity] -> Activity
scheduleActivity activity previousActivities =
  let startTimes = generateStartTimes activity
      suitableStartTimes = filter (\start -> not $ any (\prev -> conflictFound activity prev) previousActivities) startTimes
      assignStartTimeHelper :: [Int] -> Activity
      assignStartTimeHelper [] = activity -- No suitable start time found, return original activity
      assignStartTimeHelper (start:rest) =
        let actualEnd = addMinutes start (duration activity)
            updatedActivity = activity { actualStart = start, actualEnd = actualEnd }
            updatedPreviousActivities = updatedActivity : previousActivities
        in if null suitableStartTimes
            then activity -- No suitable start time found, return original activity
            else if any (\prev -> conflictFound updatedActivity prev) previousActivities
              then assignStartTimeHelper rest -- Continue with the next start time option
              else updatedActivity -- Found a suitable start time
  in assignStartTimeHelper suitableStartTimes

-- Schedule activities recursively
scheduleHelper :: [Activity] -> [Activity] -> [Activity]
scheduleHelper [] scheduledActivities = scheduledActivities
scheduleHelper (activity:rest) previousActivities =
  let scheduledActivity = scheduleActivity activity previousActivities
      updatedPreviousActivities = scheduledActivity : previousActivities
      updatedScheduledActivities = scheduledActivity : scheduleHelper rest updatedPreviousActivities
  in updatedScheduledActivities

-- Main scheduling function
schedule :: [Activity] -> [Activity]
schedule activities = scheduleHelper activities []




-- testing
main :: IO ()
main = do
  let breakfast = Activity "breakfast" 830 930 30 True 0 0
      lunch = Activity "gym" 830 1130 120 True 0 0
      dinner = Activity "lunch" 1200 1300 30 True 0 0

      -- Schedule activities
      scheduledActivities = schedule [breakfast, lunch, dinner]
  putStrLn "Scheduled Activities: "
  mapM_ print scheduledActivities

