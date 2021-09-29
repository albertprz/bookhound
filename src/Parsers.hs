module Parsers(Parser, char, digit, upper, lower, alpha, alphaNum, number,
               space, tab, newLine, comma, dot, quote, doubleQuote) where

import Parser(Parser, char)
import ParserCombinators (IsMatch(..), (<|>), someTimes)


digit :: Parser Char
digit = oneOf ['0' .. '9']

upper :: Parser Char
upper = oneOf ['A' .. 'Z']

lower :: Parser Char
lower = oneOf ['a' .. 'z']

alpha :: Parser Char
alpha =  upper <|> lower

alphaNum :: Parser Char
alphaNum = alpha <|> digit

number :: (Num a, Read a) => Parser a
number = read <$> someTimes digit


space :: Parser Char
space = is ' '

tab :: Parser [Char]
tab = is "\t"

newLine :: Parser [Char]
newLine = is "\n"

comma :: Parser Char
comma = is ','

dot :: Parser Char
dot = is '.'

quote :: Parser Char
quote = is '\''

doubleQuote :: Parser Char
doubleQuote = is '"'
