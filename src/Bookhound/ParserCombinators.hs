{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Bookhound.ParserCombinators (IsMatch(..), satisfy,
                          times, maybeTimes, anyTimes, someTimes, multipleTimes,
                          within, maybeWithin, withinBoth, maybeWithinBoth,
                          anySepBy, someSepBy, multipleSepBy, sepByOps, sepByOp,
                          (<?>), (<#>), (->>-), (|?), (|*), (|+), (|++))  where

import Bookhound.Parser (ParseError (..), Parser, allOf, anyChar, anyOf, except,
                         satisfy, withError)

import Bookhound.Utils.Applicative (extract)
import Bookhound.Utils.List        (hasMultiple, hasSome)
import Bookhound.Utils.String      (ToString (..))
import Control.Applicative
import Control.Monad.Error.Class   (MonadError (..))

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
  is      = isMatch (==) anyChar
  isNot   = isMatch (/=) anyChar
  inverse = except anyChar

instance   IsMatch String where
  is      = traverse is
  isNot   = traverse is
  inverse = except (anyChar |*)

instance {-# OVERLAPPABLE #-} (Num a, Read a, Show a) => IsMatch a where
  is n      = read <$> (is . show) n
  isNot n   = read <$> (isNot . show) n
  inverse p = read <$> inverse (show <$> p)


isMatch :: (Char -> Char -> Bool) -> Parser Char -> Char -> Parser Char
isMatch cond parser c1 =
  do c2 <- parser
     if cond c1 c2
       then pure c2
       else throwError $ UnexpectedChar c2


 -- Frequency combinators
times :: Int -> Parser a  -> Parser [a]
times n p = sequence $ p <$ [1 .. n]

maybeTimes :: Parser a -> Parser (Maybe a)
maybeTimes = optional

anyTimes :: Parser a -> Parser [a]
anyTimes p = (p >>= \x -> (x :) <$> anyTimes p) <|> pure []

someTimes :: Parser a -> Parser [a]
someTimes = satisfy hasSome . anyTimes

multipleTimes :: Parser a -> Parser [a]
multipleTimes = satisfy hasMultiple . anyTimes


-- Within combinators
withinBoth :: Parser a -> Parser b -> Parser c -> Parser c
withinBoth = extract

maybeWithinBoth :: Parser a -> Parser b -> Parser c -> Parser c
maybeWithinBoth p1 p2 = withinBoth (p1 |?) (p2 |?)

within :: Parser a -> Parser b -> Parser b
within p = withinBoth p p

maybeWithin :: Parser a -> Parser b -> Parser b
maybeWithin p = within (p |?)


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
                    pure (fst <$> y, x : (snd <$> y))

sepByOp :: Parser a -> Parser b -> Parser (a, [b])
sepByOp sep p = first head <$> sepByOps sep p


-- Parser Binary Operators
infixl 6 <#>
(<#>) :: Parser a -> Int -> Parser [a]
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
