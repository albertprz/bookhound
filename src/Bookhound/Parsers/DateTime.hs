module Bookhound.Parsers.DateTime (date, time, timeZoneOffset, localDateTime, offsetDateTime, dateTime, year, day, month, hour, minute, second) where

import Bookhound.Parser            (Parser, satisfy, withErrorN)
import Bookhound.ParserCombinators (IsMatch (..), between, (<#>), (|+), (|?))
import Bookhound.Parsers.Char      (colon, dash, digit, dot, plus)
import Control.Applicative

import Data.Maybe (fromMaybe)
import Data.Time  (Day, LocalTime (..), TimeOfDay (..), TimeZone,
                   ZonedTime (..), fromGregorian, minutesToTimeZone)


date :: Parser Day
date = withErrorN (-1) "Date" $
  fromGregorian <$> year <*> between dash month <*> day


time :: Parser TimeOfDay
time = withErrorN (-1) "Time" $
  do h <- hour
     m <- colon *> minute
     s <- colon *> second
     decimals <- ((dot *> secondDecimals) |?)
     pure $ TimeOfDay h m $ read (show s <> foldMap ("." ++) decimals)


timeZoneOffset :: Parser TimeZone
timeZoneOffset = withErrorN (-1) "Timezone Offset" $
  do pos <- (True <$ plus) <|> (False <$ dash)
     h <- hour
     m <- fromMaybe 0 <$> (minute |?)
     pure $ minutesToTimeZone $ (if pos then 1 else (-1)) * (h * 60 + m)


localDateTime :: Parser LocalTime
localDateTime = withErrorN (-1) "Local DateTime" $
  LocalTime <$> (date <* oneOf ['T', 't', ' ']) <*> time


offsetDateTime :: Parser ZonedTime
offsetDateTime = withErrorN (-1) "Offset DateTime" $
  ZonedTime <$> localDateTime <*> (is ' ' *> timeZoneOffset)


dateTime :: Parser ZonedTime
dateTime = withErrorN (-1) "DateTime" $
  ((`ZonedTime` minutesToTimeZone 0) <$> localDateTime <* is 'Z')
  <|> offsetDateTime



year :: Parser Integer
year = read <$> digit <#> 4

day :: Parser Int
day = withErrorN (-1) "Day" $ satisfy (range 1 31) $ read <$> digit <#> 2

month :: Parser Int
month = withErrorN (-1) "Month" $ satisfy (range 1 12) $ read <$> digit <#> 2

hour :: Parser Int
hour = withErrorN (-1) "Hour" $ satisfy (range 0 23) $ read <$> digit <#> 2

minute :: Parser Int
minute = withErrorN (-1) "Minute" $ satisfy (range 0 59) $ read <$> digit <#> 2

second :: Parser Int
second = withErrorN (-1) "Second" $ satisfy (range 0 59) $ read <$> digit <#> 2

secondDecimals :: Parser String
secondDecimals = withErrorN (-1) "Pico Seconds" $ satisfy ((<= 12) . length) (digit |+)




range :: Ord a => a -> a -> a -> Bool
range mn mx x = x >= mn && x <= mx
