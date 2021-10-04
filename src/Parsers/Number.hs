{-# LANGUAGE PostfixOperators #-}

module Parsers.Number where

import Parser (Parser)
import ParserCombinators (IsMatch(..), (>>>), (<|>), (|*), (|+), (|?))
import Parsers.Char (digit, dot, dash)


posInt :: Parser Integer
posInt = read <$> (digit |+)

negInt :: Parser Integer
negInt = read <$> dash >>> (digit |+)

int :: Parser Integer
int = negInt <|> posInt

double :: Parser Double
double = read <$> int >>> (decimals |?) >>> (exp |?) where

  decimals = dot >>> posInt
  exp      = oneOf ['e', 'E'] >>> int
