{-# OPTIONS_GHC -fno-warn-orphans #-}

module Bookhound.Utils.DateTime where

import Data.Time (LocalTime (..), ZonedTime (..))


instance Eq ZonedTime where
  (==) x y = (zonedTimeToLocalTime x == zonedTimeToLocalTime y) &&
             (zonedTimeZone x == zonedTimeZone y)

instance Ord ZonedTime where
  compare x y = compare (zonedTimeToLocalTime x) (zonedTimeToLocalTime y)


showDateTime :: ZonedTime -> String
showDateTime (ZonedTime (LocalTime date time) offset) =
  show date <> "T" <> show time
  <> take 3 (show offset) <> ":" <> drop 3 (show offset)
