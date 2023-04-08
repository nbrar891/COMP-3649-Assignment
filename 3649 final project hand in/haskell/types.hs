module Types where

data Activity = Activity {
  name :: String,
  startRange :: Int,
  endRange :: Int,
  duration :: Int,
  allowed :: Bool,
  actualStart :: Int,
  actualEnd :: Int
} deriving (Show, Eq)