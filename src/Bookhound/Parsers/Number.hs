module Bookhound.Parsers.Number (int, double, posInt, negInt, unsignedInt, hexInt, octInt, intLike) where

import Bookhound.Parser            (ParseError (..), Parser, errorParser,
                                    withError)
import Bookhound.ParserCombinators (IsMatch (..), (<|>), (->>-), (|+), (|?))
import Bookhound.Parsers.Char      (dash, digit, dot, plus)


hexInt :: Parser Integer
hexInt = withError "Hex Int"
 $ read <$> (is "0x" ->>-
             ((digit <|> oneOf ['A' .. 'F'] <|> oneOf ['a' .. 'f']) |+))

octInt :: Parser Integer
octInt = withError "Oct Int"
  $ read <$> (is "0o" ->>- (oneOf ['0' .. '7'] |+))

unsignedInt :: Parser Integer
unsignedInt = withError "Unsigned Int"
  $ read <$> (digit |+)

posInt :: Parser Integer
posInt = withError "Positive Int"
  $ read <$> (plus |?) ->>- (digit |+)

negInt :: Parser Integer
negInt = withError "Negative Int"
  $ read <$> dash ->>- (digit |+)

int :: Parser Integer
int = withError "Int" $ negInt <|> posInt

intLike :: Parser Integer
intLike = parser <|> int
  where
    parser = do n1      <- show <$> int
                n2      <- show <$> (dot *> unsignedInt)
                expNum  <- oneOf ['e', 'E'] *> int

                if length n1 + length n2 <= fromInteger expNum then
                  pure . read $ n1 <> "." <> n2 <> "E" <> show expNum
                else
                  errorParser $ NoMatch "Int Like"


double :: Parser Double
double = withError "Double"
  $ read <$> int ->>- (decimals |?) ->>- (expn |?) where

  decimals = dot ->>- unsignedInt
  expn      = oneOf ['e', 'E'] ->>- int
