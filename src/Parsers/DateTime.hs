{-# LANGUAGE PostfixOperators #-}

module Parsers.DateTime where

import Parser(Parser(..), check)
import ParserCombinators (IsMatch(..), (<|>), (<#>), (|?), (|*), (|+), within)
import Parsers.Char (digit, dash, colon)

import Data.Time (Day, LocalTime(..), TimeOfDay(..), TimeZone, ZonedTime(..),
                  fromGregorian, minutesToTimeZone)
import Data.Maybe (fromMaybe)


year :: Parser Integer
year = read <$> digit <#> 4

day :: Parser Int
day = check "day" (range 1 31) $ read <$> digit <#> 2

month :: Parser Int
month = check "month" (range 1 12) $ read <$> digit <#> 2

hour :: Parser Int
hour = check "hour" (range 0 23) $ read <$> digit <#> 2

minute :: Parser Int
minute = check "minute" (range 0 59) $ read <$> digit <#> 2

second :: Parser Int
second = check "second" (range 0 59) $ read <$> digit <#> 2

secondDecimals :: Parser Integer
secondDecimals = read <$> check "pico seconds" ((<= 12) . length) (digit |+)



date :: Parser Day
date = do y <- year
          m <- within dash month
          d <- day
          pure $ fromGregorian y m d


time :: Parser TimeOfDay
time = do h <- hour
          min <- colon *> minute
          s <- colon *> second
          decimals <- fromMaybe (toInteger 0) <$> ((colon *> secondDecimals) |?)
          pure $ TimeOfDay h min $ read (show s ++ "." ++ show decimals)


timeZoneOffset :: Parser TimeZone
timeZoneOffset = do h <- hour
                    min <- fromMaybe 0 <$> ((colon *> minute) |?)
                    pure $ minutesToTimeZone $ h * 60 + min

localDateTime :: Parser LocalTime
localDateTime = do d <- date
                   oneOf ['T', 't']
                   t <- time
                   pure $ LocalTime d t

offsetDateTime :: Parser ZonedTime
offsetDateTime = do localTime <- localDateTime
                    offset    <- dash *> timeZoneOffset
                    pure $ ZonedTime localTime offset

dateTime :: Parser ZonedTime
dateTime = ((`ZonedTime` minutesToTimeZone 0) <$> localDateTime <* is 'Z') <|>
            offsetDateTime


range :: Ord a => a -> a -> a -> Bool
range min max x = x >= min && x <= max
