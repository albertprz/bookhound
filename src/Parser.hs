module Parser (Parser(parse), ParseResult(..), ParseError(..), char, anyOf, allOf, isMatch, check) where

type Input = String

newtype Parser a = P { parse :: Input -> ParseResult a}

data ParseResult a = Result Input a | Error ParseError
  deriving Eq

data ParseError = UnexpectedEof       | ExpectedEof Input       |
                  UnexpectedChar Char | UnexpectedString String |
                  NoMatch String
  deriving (Eq, Show)


instance Show a => Show (ParseResult a) where
  show (Result i a)                 = "Result" ++ " >" ++ i ++ "< " ++ show a
  show (Error UnexpectedEof)        = "Unexpected end of stream"
  show (Error (ExpectedEof i))      = "Expected end of stream, but got >" ++ show i ++ "<"
  show (Error (UnexpectedChar c))   = "Unexpected character: " ++ [c]
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


errorParser :: ParseError -> Parser a
errorParser = P . const . Error


anyOf :: [Parser a] -> Parser a
anyOf [] = errorParser UnexpectedEof
anyOf [x] = x
anyOf ((P p):rest) = P (
  \i -> case p i of
    result @ (Result _ _) -> result
    error  @ (Error _)    -> parse (anyOf rest) i)


allOf :: [Parser a] -> Parser a
allOf [] = errorParser UnexpectedEof
allOf [x] = x
allOf ((P p):rest) = P (
  \i -> case p i of
    result @ (Result _ _) -> parse (allOf rest) i
    error  @ (Error _)    -> error)

char :: Parser Char
char = P parseIt where
  parseIt [] = Error UnexpectedEof
  parseIt (char:rest) = Result rest char


isMatch :: Parser Char -> Char -> Parser Char
isMatch parser c1 = do
  c2 <- parser
  let next = if c1 == c2
             then pure
             else const . errorParser $ UnexpectedChar c2
  next c1

check :: (a -> Bool) -> String -> Parser a -> Parser a
check cond condName parser = do
  c2 <- parser
  let next = if cond c2
             then pure c2
             else errorParser $ NoMatch condName
  next
