module Parsers.Number (int, double, posInt, negInt, unsignedInt, hexInt, octInt, intLike) where

import Parser            (ParseError (..), Parser, errorParser)
import ParserCombinators (IsMatch (..), (<|>), (>>>), (|+), (|?))
import Parsers.Char      (dash, digit, dot, plus)


hexInt :: Parser Integer
hexInt = read <$> (is "0x" >>> ((digit <|> oneOf ['A' .. 'F'] <|> oneOf ['a' .. 'f']) |+))

octInt :: Parser Integer
octInt = read <$> (is "0o" >>> (oneOf ['0' .. '7'] |+))

unsignedInt :: Parser Integer
unsignedInt = read <$> (digit |+)

posInt :: Parser Integer
posInt = read <$> (plus |?) >>> (digit |+)

negInt :: Parser Integer
negInt = read <$> dash >>> (digit |+)

int :: Parser Integer
int = negInt <|> posInt

intLike :: Parser Integer
intLike = parser <|> int  where

  parser = do n1      <- show <$> int
              n2      <- show <$> (dot *> unsignedInt)
              expNum  <- oneOf ['e', 'E'] *> int

              if length n1 + length n2 <= fromInteger expNum then
                pure . read $ n1 ++ "." ++ n2 ++ "E" ++ show expNum
              else
                errorParser $ NoMatch "intLike"


double :: Parser Double
double = read <$> int >>> (decimals |?) >>> (expn |?) where

  decimals = dot >>> unsignedInt
  expn      = oneOf ['e', 'E'] >>> int
