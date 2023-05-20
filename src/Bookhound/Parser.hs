module Bookhound.Parser (Parser, ParseResult, ParseError(..), runParser, errorParser,
               andThen, exactly, isMatch, check, except, anyOf, allOf, char,
               withTransform, withError) where

import Control.Applicative (liftA2)
import Control.Monad       (join)
import Data.Either         (fromRight)
import Data.List           (find)
import Data.Maybe          (isJust)
import Data.Text           (Text, pack, uncons, unpack)

type Input = Text

data Parser a
  = P
      { parse     :: Input -> ParseResult a
      , transform :: forall b. Maybe (Parser b -> Parser b)
      }

data ParseResult a
  = Result Input a
  | Error ParseError
  deriving (Eq)

data ParseError
  = UnexpectedEof
  | ExpectedEof Input
  | UnexpectedChar Char
  | UnexpectedString String
  | NoMatch String
  deriving (Eq, Show)


instance Show a => Show (ParseResult a) where
  show (Result i a)                 = "Pending: " <> " >" <> unpack i <> "< " <>
                                      "\n\nResult: \n" <> show a
  show (Error UnexpectedEof)        = "Unexpected end of stream"
  show (Error (ExpectedEof i))      = "Expected end of stream, but got >"
                                       <> unpack i <> "<"
  show (Error (UnexpectedChar c))   = "Unexpected char: "   <> "[" <> show c <> "]"
  show (Error (UnexpectedString s)) = "Unexpected string: " <> "[" <> show s <> "]"
  show (Error (NoMatch s))          = "Did not match condition: " <> s


instance Functor ParseResult where
  fmap f (Result i a) = Result i (f a)
  fmap _ (Error pe)   = Error pe


instance Functor Parser where
  fmap f (P p t) = applyTransform t $ mkParser (fmap f . p)

instance Applicative Parser where
  pure a      = mkParser (`Result` a)
  liftA2 f (P p t) mb@(P _ t') =
    applyTransform (findJust t t') combinedParser
    where
      combinedParser = mkParser
        \x -> case p x of
          Result i a -> parse ((f a) <$> mb) i
          Error pe   -> Error pe

instance Monad Parser where
  (>>=) (P p t) f = applyTransform t combinedParser
    where
      combinedParser = mkParser
        \x -> case  p x of
          Result i a -> parse (f a) i
          Error pe   -> Error pe

runParser :: Parser a -> Input -> Either ParseError a
runParser p i = toEither $ parse (exactly p) i
  where
    toEither = \case
      Error pe   -> Left pe
      Result _ a -> Right a

errorParser :: ParseError -> Parser a
errorParser = mkParser . const . Error


char :: Parser Char
char = mkParser $
   maybe (Error UnexpectedEof) (\(ch, rest) -> Result rest ch) . uncons


andThen :: Parser String -> Parser a -> Parser a
andThen p1 p2@(P _ t) = applyTransform t $
  P (\i -> parse p2 $ fromRight i $ pack <$> runParser p1 i) t


exactly :: Parser a -> Parser a
exactly (P p t) = applyTransform t $ mkParser (
  \x -> case p x of
    result@(Result i _) | i == mempty -> result
    Result i _                        -> Error $ ExpectedEof i
    err@(Error _)                     -> err
  )

anyOf :: [Parser a] -> Parser a
anyOf ps = anyOfHelper ps Nothing

allOf :: [Parser a] -> Parser a
allOf ps = allOfHelper ps Nothing


anyOfHelper :: [Parser a] -> (forall b. Maybe (Parser b -> Parser b)) -> Parser a
anyOfHelper [] _  = errorParser $ NoMatch "anyOf"
anyOfHelper [p] _ = p
anyOfHelper ((P p t) : rest) t' = applyTransform (findJust t t') $
  mkParser (
   \x -> case p x of
    result@(Result _ _) -> result
    Error _             -> parse (anyOfHelper rest t) x
  )



allOfHelper :: [Parser a] -> (forall b. Maybe (Parser b -> Parser b)) -> Parser a
allOfHelper [] _ = errorParser $ NoMatch "allOf"
allOfHelper [p] _ = p
allOfHelper ((P p t) : rest) t' = applyTransform (findJust t t') $
  mkParser (
   \x -> case p x of
    Result _ _    -> parse (allOfHelper rest t) x
    err@(Error _) -> err
  )



isMatch :: (Char -> Char -> Bool) -> Parser Char -> Char -> Parser Char
isMatch cond parser c1 =
  do c2 <- parser
     if cond c1 c2
       then pure c2
       else errorParser $ UnexpectedChar c2

check :: String -> (a -> Bool) -> Parser a -> Parser a
check condName cond parser =
  do c2 <- parser
     if cond c2
       then pure c2
       else  errorParser $ NoMatch condName


except :: Show a => Parser a -> Parser a -> Parser a
except (P p t) (P p' _) = applyTransform t $ mkParser (
  \x -> case p' x of
    Result _ a -> Error $ UnexpectedString (show a)
    Error _    -> p x
  )


withError :: String -> Parser a -> Parser a
withError str parser@(P p _) = parser { parse =
  \i -> case p i of
    r@(Result _ _) -> r
    Error _        -> Error $ NoMatch str
  }

withTransform :: (forall b. Parser b -> Parser b) -> Parser a -> Parser a
withTransform f = applyTransform $ Just f


applyTransform :: (forall a. Maybe (Parser a -> Parser a)) -> Parser b -> Parser b
applyTransform f p =  maybe p (\f' -> (f' p) {transform = f}) f

mkParser :: (Input -> ParseResult a) -> Parser a
mkParser p = P {parse = p, transform = Nothing}

findJust :: forall a. Maybe a -> Maybe a -> Maybe a
findJust ma mb = join $ find isJust ([ma, mb] :: [Maybe a])
