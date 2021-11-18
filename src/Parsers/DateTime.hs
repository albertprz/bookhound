module Parsers.DateTime (date, time, timeZoneOffset, localDateTime, offsetDateTime,
                         dateTime, year, day, month, hour, minute, second) where

import Parser(Parser(..), check)
import ParserCombinators (IsMatch(..), (<|>), (<#>), (|?), (|+), within)
import Parsers.Char (digit, dash, colon, plus)

import Data.Time (Day, LocalTime(..), TimeOfDay(..), TimeZone, ZonedTime(..),
                  fromGregorian, minutesToTimeZone)
import Data.Maybe (fromMaybe)


date :: Parser Day
date = fromGregorian <$> year <*> within dash month <*> day


time :: Parser TimeOfDay
time = do h <- hour
          m <- colon *> minute
          s <- colon *> second
          decimals <- fromMaybe 0 <$> ((colon *> secondDecimals) |?)
          pure $ TimeOfDay h m $ read (show s ++ "." ++ show decimals)


timeZoneOffset :: Parser TimeZone
timeZoneOffset = do pos <- (True <$ plus) <|> (False <$ dash)
                    h <- hour
                    m <- fromMaybe 0 <$> ((colon *> minute) |?)
                    pure $ minutesToTimeZone $ (if pos then 1 else (-1)) * (h * 60 + m)

localDateTime :: Parser LocalTime
localDateTime = LocalTime <$> (date <* oneOf ['T', 't']) <*> time

offsetDateTime :: Parser ZonedTime
offsetDateTime = ZonedTime <$> localDateTime <*> timeZoneOffset

dateTime :: Parser ZonedTime
dateTime = ((`ZonedTime` minutesToTimeZone 0) <$> localDateTime <* is 'Z') <|>
            offsetDateTime



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





range :: Ord a => a -> a -> a -> Bool
range mn mx x = x >= mn && x <= mx
