{-# LANGUAGE FlexibleInstances, IncoherentInstances, PostfixOperators #-}

module ParserCombinators  where

import Parser (Parser, char, isMatch, check, anyOf, allOf, except)
import Utils.FoldableOps (hasSome, hasMany)
import Utils.StringOps (ToString(..))
import Utils.MonadOps (extract)

import Data.Maybe (listToMaybe, maybeToList)
import Data.List (isInfixOf)


class IsMatch a where
  is :: a -> Parser a
  isNot :: a -> Parser a
  oneOf :: [a] -> Parser a
  noneOf :: [a] -> Parser a
  inverse :: Parser a -> Parser a

  oneOf xs = anyOf $ is <$> xs
  noneOf xs = allOf $ isNot <$> xs


instance IsMatch Char where
  is = isMatch (==) char
  isNot = isMatch (/=) char
  inverse = except char

instance IsMatch String where
  is = traverse is
  isNot = traverse isNot
  inverse = except (char |*)

instance IsMatch Integer where
  is n = read <$> (is . show) n
  isNot n = read <$> (isNot . show) n
  inverse p = read <$> inverse (show <$> p)

instance IsMatch Int where
  is n = read <$> (is . show) n
  isNot n = read <$> (isNot . show) n
  inverse p = read <$> inverse (show <$> p)

instance IsMatch Double where
  is n = read <$> (is . show) n
  isNot n = read <$> (isNot . show) n
  inverse p = read <$> inverse (show <$> p)



-- Condition combinators
satisfies :: Parser a -> (a -> Bool) -> Parser a
satisfies parser cond = check "satisfies" cond parser

contains :: Eq a => Parser [a] -> [a] -> Parser [a]
contains p str = check "contains" (isInfixOf str) p

notContains :: Eq a => Parser [a] -> [a] -> Parser [a]
notContains p str = check "notContains" (isInfixOf str) p


-- Frequency combinators
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


-- Within combinators
within :: Parser a -> Parser b -> Parser b
within p = extract p p

maybeWithin :: Parser a -> Parser b -> Parser b
maybeWithin p = within (p |?)

withinBoth :: Parser a -> Parser b -> Parser c -> Parser c
withinBoth = extract

maybeWithinBoth :: Parser a -> Parser b -> Parser c -> Parser c
maybeWithinBoth p1 p2 = extract (p1 |?) (p2 |?)


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
