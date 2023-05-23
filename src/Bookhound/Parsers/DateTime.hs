module Bookhound.Parsers.DateTime (date, time, timeZoneOffset, localDateTime, offsetDateTime, dateTime, year, day, month, hour, minute, second) where

import Bookhound.Parser            (Parser, check, withErrorN)
import Bookhound.ParserCombinators (IsMatch (..), within, (<#>), (<|>), (|+),
                                    (|?))
import Bookhound.Parsers.Char      (colon, dash, digit, plus)

import Data.Maybe (fromMaybe)
import Data.Time  (Day, LocalTime (..), TimeOfDay (..), TimeZone,
                   ZonedTime (..), fromGregorian, minutesToTimeZone)


date :: Parser Day
date = withErrorN (-1) "Date" $
  fromGregorian <$> year <*> within dash month <*> day


time :: Parser TimeOfDay
time = withErrorN (-1) "Time" $
  do h <- hour
     m <- colon *> minute
     s <- colon *> second
     decimals <- fromMaybe 0 <$> ((colon *> secondDecimals) |?)
     pure $ TimeOfDay h m $ read (show s <> "." <> show decimals)


timeZoneOffset :: Parser TimeZone
timeZoneOffset = withErrorN (-1) "Timezone Offset" $
  do pos <- (True <$ plus) <|> (False <$ dash)
     h <- hour
     m <- fromMaybe 0 <$> ((colon *> minute) |?)
     pure $ minutesToTimeZone $ (if pos then 1 else (-1)) * (h * 60 + m)


localDateTime :: Parser LocalTime
localDateTime = withErrorN (-1) "Local DateTime" $
  LocalTime <$> (date <* oneOf ['T', 't']) <*> time


offsetDateTime :: Parser ZonedTime
offsetDateTime = withErrorN (-1) "Offset DateTime" $
  ZonedTime <$> localDateTime <*> timeZoneOffset


dateTime :: Parser ZonedTime
dateTime = withErrorN (-1) "DateTime" $
  ((`ZonedTime` minutesToTimeZone 0) <$> localDateTime <* is 'Z')
  <|> offsetDateTime



year :: Parser Integer
year = read <$> digit <#> 4

day :: Parser Int
day = check "Day" (range 1 31) $ read <$> digit <#> 2

month :: Parser Int
month = check "Month" (range 1 12) $ read <$> digit <#> 2

hour :: Parser Int
hour = check "Hour" (range 0 23) $ read <$> digit <#> 2

minute :: Parser Int
minute = check "Minute" (range 0 59) $ read <$> digit <#> 2

second :: Parser Int
second = check "Second" (range 0 59) $ read <$> digit <#> 2

secondDecimals :: Parser Integer
secondDecimals = read <$> check "Pico Seconds" ((<= 12) . length) (digit |+)




range :: Ord a => a -> a -> a -> Bool
range mn mx x = x >= mn && x <= mx
