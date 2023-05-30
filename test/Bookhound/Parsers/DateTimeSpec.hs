module Bookhound.Parsers.DateTimeSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Instances.Text ()
import Test.QuickCheck.Instances.Time ()

import Bookhound.Parser
import Bookhound.Parsers.DateTime
import Data.Text                  (pack)

import Data.Time (Day, LocalTime, TimeOfDay, TimeZone (..), ZonedTime (..),
                  zonedTimeToUTC)


spec :: Spec
spec = do

  describe "date" $

    prop "parses a date" $
      \(x :: Day) ->
        runParser date (pack $ show x)
       `shouldBe`
        Right x

  describe "time" $

    prop "parses a time" $
      \(x :: TimeOfDay) ->
        runParser time (pack $ show x)
       `shouldBe`
        Right x

  describe "timeZoneOffset" $

    prop "parses a timezone offset" $
      \(x :: TimeZone) ->
        runParser timeZoneOffset (pack $ show $ normalizeTimeZone x)
       `shouldBe`
        Right (normalizeTimeZone x)

  describe "localDateTime" $

    prop "parses a local datetime" $
      \(x :: LocalTime) ->
        runParser localDateTime (pack $ show x)
       `shouldBe`
        Right x

  describe "DateTime" $

    prop "parses a datetime" $
      \(x :: ZonedTime) ->
        (zonedTimeToUTC <$> runParser dateTime
         (pack $ show $ normalizeZonedTime x))
       `shouldBe`
        Right (zonedTimeToUTC $ normalizeZonedTime x)



normalizeTimeZone :: TimeZone -> TimeZone
normalizeTimeZone z =
  z {timeZoneName = "", timeZoneSummerOnly = False}

normalizeZonedTime :: ZonedTime -> ZonedTime
normalizeZonedTime t@(ZonedTime {zonedTimeZone = x}) =
  t {zonedTimeZone = normalizeTimeZone x}
