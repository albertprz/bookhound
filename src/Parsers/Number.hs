{-# LANGUAGE PostfixOperators #-}

module Parsers.Number where

import Parser (Parser)
import ParserCombinators (IsMatch(..), (>>>), (<|>), (|*), (|+), (|?))
import Parsers.Char (digit, dot, dash, plus)


hexInt :: Parser Integer
hexInt = read <$> (is "0x" *> (digit <|> oneOf ['A' .. 'F'] <|> oneOf ['a' .. 'f'] |+))

octInt :: Parser Integer
octInt = read <$> (is "0o" *> (digit |+))

unsignedInt :: Parser Integer
unsignedInt = read <$> (digit |+)

posInt :: Parser Integer
posInt = read <$> (plus |?) >>> (digit |+)

negInt :: Parser Integer
negInt = read <$> dash >>> (digit |+)

int :: Parser Integer
int = negInt <|> posInt


double :: Parser Double
double = read <$> int >>> (decimals |?) >>> (exp |?) where

  decimals = dot >>> unsignedInt
  exp      = oneOf ['e', 'E'] >>> int
