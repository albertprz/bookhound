{-# LANGUAGE UndecidableInstances #-}

module ParserCombinators (IsMatch(..), satisfies, contains, notContains,
                          times, maybeTimes, anyTimes, someTimes, manyTimes,
                          within, maybeWithin, withinBoth, maybeWithinBoth,
                          anySeparatedBy, someSeparatedBy, manySeparatedBy,
                          (<|>), (<&>), (<#>), (>>>), (|?), (|*), (|+), (|++))  where

import Parser (Parser, char, isMatch, check, anyOf, allOf, except)
import Utils.Foldable (hasSome, hasMany)
import Utils.String (ToString(..))
import Utils.Applicative (extract)
import qualified Data.Foldable as Foldable

import Data.Maybe (listToMaybe)
import Data.List (isInfixOf)


class IsMatch a where
  is      :: a -> Parser a
  isNot   :: a -> Parser a
  oneOf   :: [a] -> Parser a
  noneOf  :: [a] -> Parser a
  inverse :: Parser a -> Parser a

  oneOf xs  = anyOf $ is <$> xs
  noneOf xs = allOf $ isNot <$> xs


instance IsMatch Char where
  is      = isMatch (==) char
  isNot   = isMatch (/=) char
  inverse = except char

instance IsMatch String where
  is      = traverse is
  isNot   = traverse isNot
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


 -- Frequency combinators
times :: Integer -> Parser a  -> Parser [a]
times n p = sequence $ p <$ [1 .. n]


maybeTimes :: Parser a -> Parser (Maybe a)
maybeTimes = (listToMaybe <$>) . check "maybeTimes" (not . hasMany) . anyTimes

anyTimes :: Parser a -> Parser [a]
anyTimes p = ((:) <$> p <*> anyTimes p) <|> pure []

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


-- Separated by combinators
separatedBy :: (Parser b -> Parser (Maybe b)) -> (Parser b -> Parser [b])
                -> Parser a -> Parser b -> Parser [b]
separatedBy freq1 freq2 sep p = (++) <$> (Foldable.toList <$> freq1 p)
                                     <*> freq2 (sep *> p)

anySeparatedBy :: Parser a -> Parser b -> Parser [b]
anySeparatedBy = separatedBy (|?) (|*)

someSeparatedBy :: Parser a -> Parser b -> Parser [b]
someSeparatedBy = separatedBy (fmap Just) (|*)

manySeparatedBy :: Parser a -> Parser b -> Parser [b]
manySeparatedBy = separatedBy (fmap Just) (|+)

-- Parser Binary Operators
infixl 3 <|>
(<|>) :: Parser a -> Parser a -> Parser a
(<|>) p1 p2 = anyOf [p1, p2]

infixl 3 <&>
(<&>) :: Parser a -> Parser a -> Parser a
(<&>) p1 p2 = allOf [p1, p2]

infixl 6 <#>
(<#>) :: Parser a -> Integer -> Parser [a]
(<#>) = flip times

infixl 6 >>>
(>>>) :: (ToString a, ToString b) => Parser a -> Parser b -> Parser String
(>>>) p1 p2 = (++) <$> (toString <$> p1)
                   <*> (toString <$> p2)


-- Parser Unary Operators
(|?) :: Parser a -> Parser (Maybe a)
(|?) = maybeTimes

(|*) :: Parser a -> Parser [a]
(|*) = anyTimes

(|+) :: Parser a -> Parser [a]
(|+) = someTimes

(|++) :: Parser a -> Parser [a]
(|++) = manyTimes
