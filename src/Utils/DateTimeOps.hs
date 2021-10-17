module Utils.DateTimeOps where

import Data.Time (ZonedTime(..))


instance Eq ZonedTime where
  (==) x y = (zonedTimeToLocalTime x == zonedTimeToLocalTime y) &&
             (zonedTimeZone x == zonedTimeZone y)

instance Ord ZonedTime where
  compare x y = compare (zonedTimeToLocalTime x) (zonedTimeToLocalTime y)
