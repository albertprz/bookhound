module Bookhound.Parsers.Number (int, double, posInt, negInt, unsignedInt, hexInt, octInt, intLike) where

import Bookhound.Parser            (ParseError (..), Parser, throwError,
                                    withErrorN)
import Bookhound.ParserCombinators (IsMatch (..), (->>-), (|+), (|?))
import Bookhound.Parsers.Char      (dash, digit, dot, plus)
import Control.Applicative


hexInt :: Parser Integer
hexInt = withErrorN (-1) "Hex Int"
 $ read <$> (is "0x" ->>-
             ((digit <|> oneOf ['A' .. 'F'] <|> oneOf ['a' .. 'f']) |+))

octInt :: Parser Integer
octInt = withErrorN (-1) "Oct Int"
  $ read <$> (is "0o" ->>- (oneOf ['0' .. '7'] |+))

unsignedInt :: Parser Integer
unsignedInt = withErrorN (-1) "Unsigned Int"
  $ read <$> (digit |+)

posInt :: Parser Integer
posInt = withErrorN (-1) "Positive Int"
  $ read <$> ((plus |?) *> (digit |+))

negInt :: Parser Integer
negInt = withErrorN (-1) "Negative Int"
  $ read <$> dash ->>- (digit |+)

int :: Parser Integer
int = withErrorN (-1) "Int"
  $ negInt <|> posInt

intLike :: Parser Integer
intLike = parser <|> int
  where
    parser = do n1      <- show <$> int
                n2      <- show <$> (dot *> unsignedInt)
                expNum  <- oneOf ['e', 'E'] *> int

                if length n1 + length n2 <= fromInteger expNum then
                  pure . read $ n1 <> "." <> n2 <> "E" <> show expNum
                else
                  throwError $ NoMatch "Int Like"

double :: Parser Double
double = withErrorN (-1) "Double"
  $ read <$> (dash |?) ->>- posInt ->>- (decimals |?) ->>- (expn |?) where

  decimals = dot ->>- (digit |+)
  expn      = oneOf ['e', 'E'] ->>- int
