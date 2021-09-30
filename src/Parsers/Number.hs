{-# LANGUAGE PostfixOperators #-}

module Parsers.Number where

import ParserCombinators (Parser, (>>>), (<|>), (|*), (|+), (|?))
import Parsers.Char (digit, dot, dash)


posInt :: Parser Integer
posInt = read <$> (digit |+)

negInt :: Parser Integer
negInt = read <$> dash >>> (digit |+)

int :: Parser Integer
int = negInt <|> posInt

number :: Parser Double
number = read <$> int >>> (dot >>> posInt |?)
