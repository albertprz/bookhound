module Parser (Parser(..), ParseResult(..), ParseError(..), runParser, errorParser,
               andThen, exactly, isMatch, check, except, anyOf, allOf, char)  where

import Data.Either (fromRight)
import Data.Functor((<&>))

type Input = String

newtype Parser a = P { parse :: Input -> ParseResult a}

data ParseResult a = Result Input a | Error ParseError
  deriving Eq

data ParseError = UnexpectedEof       | ExpectedEof Input       |
                  UnexpectedChar Char | UnexpectedString String |
                  NoMatch String
  deriving (Eq, Show)


instance Show a => Show (ParseResult a) where
  show (Result i a)                 = "Pending: " ++ " >" ++ i ++ "< " ++
                                      "\n\nResult: \n" ++ show a
  show (Error UnexpectedEof)        = "Unexpected end of stream"
  show (Error (ExpectedEof i))      = "Expected end of stream, but got >" ++ show i ++ "<"
  show (Error (UnexpectedChar c))   = "Unexpected character: " ++ show c
  show (Error (UnexpectedString s)) = "Unexpected string: " ++ show s
  show (Error (NoMatch s))          = "Did not match condition: " ++ s


instance Functor ParseResult where
  fmap f (Result i a) = Result i (f a)
  fmap _ (Error pe) = Error pe


instance Functor Parser where
  fmap f (P p) = P (fmap f . p)

instance Applicative Parser where
  pure a = P (`Result` a)
  (<*>) mf ma = mf >>= (ma <&>)

instance Monad Parser where
  (>>=) (P p) f = P (
    \x -> case p x of
      Result i a -> parse (f a) i
      Error pe -> Error pe)


runParser :: Parser a -> Input -> Either ParseError a
runParser p i = toEither $ parse p i  where

  toEither = \case
    Error pe -> Left pe
    Result input a -> if null input then Right a
                      else               Left $ ExpectedEof input

errorParser :: ParseError -> Parser a
errorParser = P . const . Error


char :: Parser Char
char = P parseIt where
  parseIt [] = Error UnexpectedEof
  parseIt (ch : rest) = Result rest ch



andThen :: Parser Input -> Parser a -> Parser a
andThen p1 p2 = P (\i -> parse p2 $ fromRight i $ runParser p1 i)


exactly :: Parser a -> Parser a
exactly (P p) = P (
  \x -> case p x of
    result@(Result "" _) -> result
    Result i _           -> Error $ ExpectedEof i
    err@(Error _)        -> err)


anyOf :: [Parser a] -> Parser a
anyOf [] = errorParser UnexpectedEof
anyOf [x] = x
anyOf ((P p) : rest) = P (
  \x -> case p x of
    result@(Result _ _) -> result
    Error _             -> parse (anyOf rest) x)


allOf :: [Parser a] -> Parser a
allOf [] = errorParser UnexpectedEof
allOf [x] = x
allOf ((P p) : rest) = P (
  \x -> case p x of
    Result i _    -> parse (allOf rest) i
    err@(Error _) -> err)


isMatch :: (Char -> Char -> Bool) -> Parser Char -> Char -> Parser Char
isMatch cond parser c1 = do
  c2 <- parser
  let next = if cond c1 c2
             then pure
             else const . errorParser $ UnexpectedChar c2
  next c2


check :: String -> (a -> Bool) -> Parser a -> Parser a
check condName cond parser = do
  c2 <- parser
  let next = if cond c2
             then pure
             else const . errorParser $ NoMatch condName
  next c2


except :: Show a => Parser a -> Parser a -> Parser a
except alt (P p) = P (
  \x -> case p x of
    Result _ a -> Error $ UnexpectedString (show a)
    Error _     -> parse alt x)
