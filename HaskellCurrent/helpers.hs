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