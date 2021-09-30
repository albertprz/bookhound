module Parsers.Char where

import qualified Internal.Parser as Parser
import ParserCombinators (Parser, IsMatch(..), (<|>))


char :: Parser Char
char = Parser.char

digit :: Parser Char
digit = oneOf ['0' .. '9']

upper :: Parser Char
upper = oneOf ['A' .. 'Z']

lower :: Parser Char
lower = oneOf ['a' .. 'z']

letter :: Parser Char
letter = upper <|> lower

alpha :: Parser Char
alpha = letter

alphaNum :: Parser Char
alphaNum = alpha <|> digit



space :: Parser Char
space = is ' '

comma :: Parser Char
comma = is ','

dot :: Parser Char
dot = is '.'

colon :: Parser Char
colon = is ':'

quote :: Parser Char
quote = is '\''

doubleQuote :: Parser Char
doubleQuote = is '"'

dash :: Parser Char
dash = is '-'

underscore :: Parser Char
underscore = is '_'
