{-# LANGUAGE FlexibleInstances, UndecidableInstances, IncoherentInstances, MonoLocalBinds  #-}

module ParserCombinators (module ParserCombinators, Parser(parse)) where

import Internal.Parser (Parser(parse), char, isMatch, check, anyOf, allOf)
import Util.FoldableOps (hasSome, hasMany)
import Util.StringOps (ToString(..))

import Data.Maybe (listToMaybe, maybeToList)


class IsMatch a where
  is :: a -> Parser a
  isNot :: a -> Parser a
  oneOf :: [a] -> Parser a
  noneOf :: [a] -> Parser a
  satisfies :: Parser a -> (a -> Bool) -> Parser a

  oneOf xs = anyOf $ is <$> xs
  noneOf xs = allOf $ isNot <$> xs
  satisfies parser cond = check "satisfies" cond parser


instance IsMatch Char where
  is = isMatch (==) char
  isNot = isMatch (/=) char

instance IsMatch String where
  is = traverse is
  isNot = traverse isNot

instance (Num a, Read a, Show a) => IsMatch a where
  is n = read <$> (is . show) n
  isNot n = read <$> (isNot . show) n



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

(>>>) :: (ToString a, ToString b) => Parser a -> Parser b -> Parser String
(>>>) p1 p2 = p1 >>= (\x -> (x ++) <$> (toString <$> p2)) . toString


-- Parser Unary Operators
(|?) :: Parser a -> Parser (Maybe a)
(|?) = maybeTimes

(|*) :: Parser a -> Parser [a]
(|*) = anyTimes

(|+) :: Parser a -> Parser [a]
(|+) = someTimes

(|++) :: Parser a -> Parser [a]
(|++) = manyTimes
