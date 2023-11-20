module Bookhound.ParserCombinators (IsMatch(..), satisfy, char, string, times, many, some, multiple,
                          between, maybeBetween, surroundedBy, maybeSurroundedBy,
                          manySepBy, someSepBy, multipleSepBy, sepByOps, sepByOp, manyEndBy, someEndBy, multipleEndBy,
                          (<?>), (<#>), (</\>), (<:>), (->>-), (|?), (|*), (|+), (|++), (||?), (||*), (||+), (||++))  where
import Bookhound.Parser (Parser, allOf, anyChar, anyOf, except, satisfy,
                         withError)

import Bookhound.Utils.List (hasMultiple, hasSome)
import Bookhound.Utils.Text (ToText (..))
import Control.Applicative  (liftA2, optional, (<|>))

import qualified Data.Foldable as Foldable
import           Data.Text     (Text, pack, unpack)
import qualified Data.Text     as Text


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

instance   IsMatch Text where
  is      = fmap pack . traverse is . unpack
  isNot   = fmap pack . traverse is . unpack
  inverse = except (anyChar ||*)


isMatch :: (Char -> Char -> Bool) -> Parser Char -> Char -> Parser Char
isMatch cond p c1 = satisfy (cond c1) p

char :: Char -> Parser Char
char = is

string :: Text -> Parser Text
string = is

 -- Frequency combinators
many :: Parser a -> Parser [a]
many p = (p >>= \x -> (x :) <$> many p)
  <|> pure []

some :: Parser a -> Parser [a]
some = satisfy hasSome . many

multiple :: Parser a -> Parser [a]
multiple = satisfy hasMultiple . many

times :: Int -> Parser a  -> Parser [a]
times n p
  | n < 1 = pure []
  | otherwise = sequence $ p <$ [1 .. n]


-- Between combinators
between :: Parser a -> Parser b -> Parser c -> Parser c
between start end p = start *> p <* end

maybeBetween :: Parser a -> Parser b -> Parser c -> Parser c
maybeBetween p1 p2 = between (p1 |?) (p2 |?)

surroundedBy :: Parser a -> Parser b -> Parser b
surroundedBy p = between p p

maybeSurroundedBy :: Parser a -> Parser b -> Parser b
maybeSurroundedBy p = surroundedBy (p |?)


-- Sep by combinators
sepBy :: (Parser b -> Parser (Maybe b)) -> (Parser b -> Parser [b])
                -> Parser a -> Parser b -> Parser [b]
sepBy freq1 freq2 sep p = (<>) <$> (Foldable.toList <$> freq1 p)
                               <*> freq2 (sep *> p)

manySepBy :: Parser a -> Parser b -> Parser [b]
manySepBy = sepBy optional many

someSepBy :: Parser a -> Parser b -> Parser [b]
someSepBy = sepBy (fmap Just) many

multipleSepBy :: Parser a -> Parser b -> Parser [b]
multipleSepBy = sepBy (fmap Just) some

sepByOps :: Parser a -> Parser b -> Parser ([a], [b])
sepByOps sep p = do x <-  p
                    y <- (|+) (sep </\> p)
                    pure (fmap fst y, x : fmap snd y)

sepByOp :: Parser a -> Parser b -> Parser (a, [b])
sepByOp sepP p = do
  x1 <- p
  sep <- sepP
  x2 <- p
  xs <- (|*) (sepP *> p)
  pure (sep, x1 : x2 : xs)

-- End by combinators
endBy
  :: forall a b
   . (Parser b -> Parser (Maybe b))
  -> (Parser b -> Parser [b])
  -> Parser a
  -> Parser b
  -> Parser [b]
endBy freq1 freq2 sep p =
  sepBy freq1 freq2 sep p <* sep

manyEndBy :: forall a b. Parser a -> Parser b -> Parser [ b]
manyEndBy = endBy optional many

someEndBy :: forall a b. Parser a -> Parser b -> Parser [ b]
someEndBy = endBy (fmap Just) many

multipleEndBy :: forall a b. Parser a -> Parser b -> Parser [ b]
multipleEndBy = endBy (fmap Just) some


-- Parser Binary Operators
infixl 6 <#>
(<#>) :: Parser a -> Int -> Parser [a]
(<#>) = flip times

infixl 6 <?>
(<?>) :: Parser a -> Text -> Parser a
(<?>) = flip withError

infixl 6 ->>-
(->>-) :: (ToText a, ToText b) => Parser a -> Parser b -> Parser Text
(->>-) p1 p2 = (<>) <$> fmap toText p1
                 <*> fmap toText p2

-- Apply Binary Operators
infixl 6 </\>
(</\>) :: Applicative f => f a -> f b -> f (a, b)
(</\>) = liftA2 (,)

infixl 6 <:>
(<:>) :: Applicative f => f a -> f [a] -> f [a]
(<:>) = liftA2 (:)

-- Parser Unary Operators
(|?) :: Parser a -> Parser (Maybe a)
(|?) = optional

(|*) :: Parser a -> Parser [a]
(|*) = many

(|+) :: Parser a -> Parser [a]
(|+) = some

(|++) :: Parser a -> Parser [a]
(|++) = multiple

(||?) :: Parser Char -> Parser Text
(||?) = fmap (maybe mempty Text.singleton) . optional

(||*) :: Parser Char -> Parser Text
(||*) = fmap pack . many

(||+) :: Parser Char -> Parser Text
(||+) = fmap pack . some

(||++) :: Parser Char -> Parser Text
(||++) = fmap pack . multiple
