module Parser where

import Data.Maybe (maybeToList)
import Data.Either (fromRight)

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
  fmap f (Error pe) = Error pe


instance Functor Parser where
  fmap f (P p) = P (fmap f . p)

instance Applicative Parser where
  pure a = P (`Result` a)
  (<*>) mf ma = mf >>= (\f -> ma >>= (pure . f))

instance Monad Parser where
  (>>=) (P p) f = P (
    \i -> case p i of
      Result i a -> parse (f a) i
      Error pe -> Error pe)


runParser :: Parser a -> Input -> Either ParseError a
runParser p i = toEither $ parse p i


toEither :: ParseResult a -> Either ParseError a
toEither result = case result of
  Error pe -> Left pe
  Result input a -> if null input then Right a
                    else               Left $ ExpectedEof input


char :: Parser Char
char = P parseIt where
  parseIt [] = Error UnexpectedEof
  parseIt (char : rest) = Result rest char


errorParser :: ParseError -> Parser a
errorParser = P . const . Error


andThen :: Parser Input -> Parser a -> Parser a
andThen p1 p2 = P (\i -> parse p2 $ fromRight i $ runParser p1 i)


exactly :: Parser a -> Parser a
exactly (P p) = P (
  \i -> case p i of
    result @ (Result "" _) -> result
    result @ (Result i _)  -> Error $ ExpectedEof i
    error  @ (Error _)     -> error)


anyOf :: [Parser a] -> Parser a
anyOf [] = errorParser UnexpectedEof
anyOf [x] = x
anyOf ((P p) : rest) = P (
  \i -> case p i of
    result @ (Result _ _) -> result
    error  @ (Error _)    -> parse (anyOf rest) i)


allOf :: [Parser a] -> Parser a
allOf [] = errorParser UnexpectedEof
allOf [x] = x
allOf ((P p) : rest) = P (
  \i -> case p i of
    result @ (Result _ _) -> parse (allOf rest) i
    error  @ (Error _)    -> error)


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
  \i -> case p i of
    result @ (Result _ a) -> Error $ UnexpectedString (show a)
    error  @ (Error _)    -> parse alt i)
