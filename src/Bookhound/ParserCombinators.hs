{-# LANGUAGE UndecidableInstances #-}

module Bookhound.ParserCombinators (IsMatch(..), satisfies, contains, notContains,
                          containsAnyOf, containsNoneOf,
                          times, maybeTimes, anyTimes, someTimes, multipleTimes,
                          within, maybeWithin, withinBoth, maybeWithinBoth,
                          anySepBy, someSepBy, multipleSepBy, sepByOp,
                          (<|>), (<?>), (<#>), (->>-), (|?), (|*), (|+), (|++))  where

import Bookhound.Utils.Applicative (extract)
import Bookhound.Utils.Foldable    (hasMultiple, hasSome)
import Bookhound.Utils.String      (ToString (..))
import Bookhound.Parser               (Parser, allOf, anyOf, char, check,
                                       except, isMatch, withError)

import Data.List  (isInfixOf)
import Data.Maybe (listToMaybe)

import           Data.Bifunctor (Bifunctor (first))
import qualified Data.Foldable  as Foldable


class IsMatch a where
  is      :: a -> Parser a
  isNot   :: a -> Parser a
  inverse :: Parser a -> Parser a
  oneOf   :: [a] -> Parser a
  noneOf  :: [a] -> Parser a

  oneOf xs  = anyOf $ is <$> xs
  noneOf xs = allOf $ isNot <$> xs


instance   IsMatch Char where
  is      = isMatch (==) char
  isNot   = isMatch (/=) char
  inverse = except char

instance   IsMatch String where
  is      = traverse (isMatch (==) char)
  isNot   = traverse (isMatch (/=) char)
  inverse = except (char |*)

instance {-# OVERLAPPABLE #-} (Num a, Read a, Show a) => IsMatch a where
  is n      = read <$> (is . show) n
  isNot n   = read <$> (isNot . show) n
  inverse p = read <$> inverse (show <$> p)


-- Condition combinators
satisfies :: (a -> Bool) -> Parser a -> Parser a
satisfies cond p = check "satisfies" cond p

contains :: Eq a => [a] -> Parser [a] -> Parser [a]
contains val p = check "contains" (isInfixOf val) p

notContains :: Eq a => [a] -> Parser [a] -> Parser [a]
notContains val p = check "notContains" (isInfixOf val) p

containsAnyOf :: (Foldable t, Eq a) => t [a] -> Parser [a] -> Parser [a]
containsAnyOf x y = foldr contains y x

containsNoneOf :: (Foldable t, Eq a) => t [a] -> Parser [a] -> Parser [a]
containsNoneOf x y = foldr notContains y x


 -- Frequency combinators
times :: Integer -> Parser a  -> Parser [a]
times n p = sequence $ p <$ [1 .. n]


maybeTimes :: Parser a -> Parser (Maybe a)
maybeTimes = (listToMaybe <$>) . check "maybeTimes" (not . hasMultiple) . anyTimes

anyTimes :: Parser a -> Parser [a]
anyTimes parser = (parser >>= \x -> (x :) <$> anyTimes parser) <|> pure []

someTimes :: Parser a -> Parser [a]
someTimes = check "someTimes" hasSome . anyTimes

multipleTimes :: Parser a -> Parser [a]
multipleTimes = check "multipleTimes" hasMultiple . anyTimes


-- Within combinators
within :: Parser a -> Parser b -> Parser b
within p = extract p p

maybeWithin :: Parser a -> Parser b -> Parser b
maybeWithin p = within (p |?)

withinBoth :: Parser a -> Parser b -> Parser c -> Parser c
withinBoth = extract

maybeWithinBoth :: Parser a -> Parser b -> Parser c -> Parser c
maybeWithinBoth p1 p2 = extract (p1 |?) (p2 |?)


-- Separated by combinators
sepBy :: (Parser b -> Parser (Maybe b)) -> (Parser b -> Parser [b])
                -> Parser a -> Parser b -> Parser [b]
sepBy freq1 freq2 sep p = (<>) <$> (Foldable.toList <$> freq1 p)
                               <*> freq2 (sep *> p)

anySepBy :: Parser a -> Parser b -> Parser [b]
anySepBy = sepBy (|?) (|*)

someSepBy :: Parser a -> Parser b -> Parser [b]
someSepBy = sepBy (fmap Just) (|*)

multipleSepBy :: Parser a -> Parser b -> Parser [b]
multipleSepBy = sepBy (fmap Just) (|+)

sepByOps :: Parser a -> Parser b -> Parser ([a], [b])
sepByOps sep p = do x <-  p
                    y <- (((,) <$> sep <*> p) |+)
                    pure $ (fst <$> y, x : (snd <$> y))

sepByOp :: Parser a -> Parser b -> Parser (a, [b])
sepByOp sep p = first head <$> sepByOps sep p


-- Parser Binary Operators
infixl 3 <|>
(<|>) :: Parser a -> Parser a -> Parser a
(<|>) p1 p2 = anyOf [p1, p2]

infixl 6 <#>
(<#>) :: Parser a -> Integer -> Parser [a]
(<#>) = flip times

infixl 6 <?>
(<?>) :: Parser a -> String -> Parser a
(<?>) = flip withError

infixl 6 ->>-
(->>-) :: (ToString a, ToString b) => Parser a -> Parser b -> Parser String
(->>-) p1 p2 = (<>) <$> (toString <$> p1)
                 <*> (toString <$> p2)


-- Parser Unary Operators
(|?) :: Parser a -> Parser (Maybe a)
(|?) = maybeTimes

(|*) :: Parser a -> Parser [a]
(|*) = anyTimes

(|+) :: Parser a -> Parser [a]
(|+) = someTimes

(|++) :: Parser a -> Parser [a]
(|++) = multipleTimes
