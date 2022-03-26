module Parser (Parser, ParseResult(..), ParseError(..), runParser, errorParser,
               andThen, exactly, isMatch, check, except, anyOf, allOf, char,
               withTransform)  where

import Data.Either (fromRight)

type Input = String

data Parser a = P { parse :: Input -> ParseResult a
                  , transform :: forall b. Parser b -> Parser b
                  }

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
  fmap f (P p t) = P (fmap f . p) t

instance Applicative Parser where
  pure a = P (`Result` a) id
  (<*>) mf ma = mf >>= (<$> ma)

instance Monad Parser where
  (>>=) (P p t) f = t $ P (
    \x -> case p x of
      Result i a -> (.parse) (f a) i
      Error pe -> Error pe)
    t

withTransform :: (forall b. Parser b -> Parser b) -> Parser a -> Parser a
withTransform f p = p{transform = f}


runParser :: Parser a -> Input -> Either ParseError a
runParser p i = toEither $ (.parse) (exactly p) i where

  toEither = \case
    Error pe -> Left pe
    Result _ a -> Right a

errorParser :: ParseError -> Parser a
errorParser = mkParser . const . Error


char :: Parser Char
char = mkParser parseIt  where
  parseIt [] = Error UnexpectedEof
  parseIt (ch : rest) = Result rest ch



andThen :: Parser Input -> Parser a -> Parser a
andThen p1 p2@(P _ t) = t $ P (\i -> (.parse) p2 $ fromRight i $ runParser p1 i) t


exactly :: Parser a -> Parser a
exactly (P p t) = t $ P (
  \x -> case p x of
    result@(Result "" _) -> result
    Result i _           -> Error $ ExpectedEof i
    err@(Error _)        -> err) t


anyOf :: [Parser a] -> Parser a
anyOf [] = errorParser UnexpectedEof
anyOf [x] = x
anyOf ((P p t) : rest) = t $ P (
  \x -> case p x of
    result@(Result _ _) -> result
    Error _             -> (.parse) (anyOf rest) x) t


allOf :: [Parser a] -> Parser a
allOf [] = errorParser UnexpectedEof
allOf [x] = x
allOf ((P p t) : rest) = t $ P (
  \x -> case p x of
    Result i _    -> (.parse) (allOf rest) i
    err@(Error _) -> err) t


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
except alt (P p t) = t $ P (
  \x -> case p x of
    Result _ a -> Error $ UnexpectedString (show a)
    Error _     -> (.parse) alt x) t


mkParser :: (Input -> ParseResult a) -> Parser a
mkParser p = P {parse = p, transform = id}
