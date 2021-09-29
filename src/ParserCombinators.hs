{-# LANGUAGE FlexibleInstances, UndecidableInstances, IncoherentInstances, PostfixOperators  #-}

module ParserCombinators(IsMatch(..), (<|>), (<&>), (<#>), (|?), (|*), (|+), (|++),
                         anyOf, allOf, times, maybeTimes, anyTimes, someTimes, manyTimes) where

import Internal.Parser (Parser(parse), char, anyOf, allOf, isMatch, check)
import Util.List (hasSome, hasMany)
import Data.Maybe (listToMaybe)


class IsMatch a where
  is :: a -> Parser a
  inSet :: [a] -> Parser a
  satisfies :: Parser a -> (a -> Bool) -> Parser a

  inSet xs = anyOf $ is <$> xs
  satisfies parser cond = check "satisfies" cond parser


instance IsMatch Char where
  is = isMatch char

instance IsMatch String where
  is = traverse is

instance (Num a, Read a, Show a) => IsMatch a where
  is n = read <$> (is . show) n



times :: Parser a -> Integer -> Parser [a]
times parser n = sequence $ parser <$ [1 .. n]

maybeTimes :: Parser a -> Parser (Maybe a)
maybeTimes = (listToMaybe <$>) . check "maybeTimes" (not . hasMany) . anyTimes

anyTimes :: Parser a -> Parser [a]
anyTimes parser = (parser >>= \x -> (x :) <$> anyTimes parser) <|> pure []

someTimes :: Parser a -> Parser [a]
someTimes = check "someTimes" hasSome . anyTimes

manyTimes :: Parser a -> Parser [a]
manyTimes = check "manyTimes" hasMany . anyTimes



-- Parser Binary Operators
(<|>) :: Parser a -> Parser a -> Parser a
(<|>) p1 p2 = anyOf [p1, p2]

(<&>) :: Parser a -> Parser a -> Parser a
(<&>) p1 p2 = allOf [p1, p2]

(<#>) :: Parser a -> Integer -> Parser [a]
(<#>) = times


-- Parser Unary Operators
(|?) :: Parser a -> Parser (Maybe a)
(|?) = maybeTimes

(|*) :: Parser a -> Parser [a]
(|*) = anyTimes

(|+) :: Parser a -> Parser [a]
(|+) = someTimes

(|++) :: Parser a -> Parser [a]
(|++) = manyTimes
