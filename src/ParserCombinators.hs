{-# LANGUAGE FlexibleInstances, UndecidableInstances, IncoherentInstances #-}

module ParserCombinators(IsMatch(..), (<|>), (<&>), (|?), (|*), (|+), (|++),
                         times, maybeTimes, anyTimes, someTimes, manyTimes) where

import Parser (Parser(parse), char, anyOf, allOf, isMatch, check)
import Data.Maybe (listToMaybe)


class IsMatch a where
  is :: a -> Parser a
  oneOf :: [a] -> Parser a

  oneOf xs = anyOf $ is <$> xs

instance IsMatch Char where
  is = isMatch char

instance IsMatch String where
  is = traverse (isMatch char)

instance (Num a, Read a, Show a) => IsMatch a where
  is n = read <$> (traverse (isMatch char) . show) n


hasSome :: [a] -> Bool
hasSome = not . null

hasMany :: [a] -> Bool
hasMany xs = all hasSome [xs, tail xs]



times :: Parser a -> Integer -> Parser [a]
times parser n = sequence $ parser <$ [1 .. n]

maybeTimes :: Parser a -> Parser (Maybe a)
maybeTimes = (listToMaybe <$>) . check (not . hasMany) "maybeTimes" . anyTimes

anyTimes :: Parser a -> Parser [a]
anyTimes parser = (parser >>= \x -> (x :) <$> anyTimes parser) <|> pure []

someTimes :: Parser a -> Parser [a]
someTimes = check hasSome "someTimes" . anyTimes

manyTimes :: Parser a -> Parser [a]
manyTimes = check hasMany "manyTimes" . anyTimes



(<|>) :: Parser a -> Parser a -> Parser a
(<|>) p1 p2 = anyOf [p1, p2]

(<&>) :: Parser a -> Parser a -> Parser a
(<&>) p1 p2 = allOf [p1, p2]

(|?) :: Parser a -> Parser (Maybe a)
(|?) = maybeTimes

(|*) :: Parser a -> Parser [a]
(|*) = anyTimes

(|+) :: Parser a -> Parser [a]
(|+) = someTimes

(|++) :: Parser a -> Parser [a]
(|++) = manyTimes
