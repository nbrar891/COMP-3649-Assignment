--helper functions for various tasks
--check allowance buggy so not using

module Helpers where

import Data.List
import Data.Char
import Types (Activity(..))


-- Sorts activities by actual end time, and if that is equal then actual start time
sortByActualEndTime :: [Activity] -> [Activity]
sortByActualEndTime = sortBy $ \a1 a2 ->
    case compare (actualEnd a1) (actualEnd a2) of
        EQ -> compare (actualStart a1) (actualStart a2)
        x -> x

-- Adds minutes to military time
addMinutes :: Int -> Int -> Int
addMinutes militaryTime minutesToAdd =
    let hours = div militaryTime 100
        minutes = mod militaryTime 100
        totalMinutes = hours * 60 + minutes + minutesToAdd
        resultHours = div totalMinutes 60
        resultMinutes = mod totalMinutes 60
    in resultHours * 100 + resultMinutes


subtractTimeMinutes :: Int -> Int -> Int
subtractTimeMinutes time1 time2 =
  let hour1 = time1 `div` 100
      minute1 = time1 `mod` 100
      hour2 = time2 `div` 100
      minute2 = time2 `mod` 100
  in (hour1 - hour2) * 60 + minute1 - minute2

checkAllowance :: Int -> Int -> Int -> Bool
checkAllowance startRange endRange duration =
  let timeRange = subtractTimeMinutes endRange startRange
  in duration <= timeRange

filterAllowedActivities :: [Activity] -> [Activity]
filterAllowedActivities activities =
  filter (\a -> allowed a) activities

splitInHalf :: [a] -> [a]
splitInHalf xs = take (length xs `div` 2) xs  


  