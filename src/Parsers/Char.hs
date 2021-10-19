module Parsers.Char where

import qualified Parser
import Parser (Parser)
import ParserCombinators (IsMatch(..), (<|>))
import Data.Data (ConstrRep(CharConstr))


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

tab :: Parser Char
tab = is '\t'

spaceOrTab :: Parser Char
spaceOrTab = space <|> tab

whiteSpace :: Parser Char
whiteSpace = space <|> tab <|> newLine

newLine :: Parser Char
newLine = is '\n'

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

plus :: Parser Char
plus = is '+'

equal :: Parser Char
equal = is '='

underscore :: Parser Char
underscore = is '_'

hashTag :: Parser Char
hashTag = is '#'

question :: Parser Char
question = is '?'



openParens :: Parser Char
openParens = is '('

closeParens :: Parser Char
closeParens = is ')'

openSquare :: Parser Char
openSquare = is '['

closeSquare :: Parser Char
closeSquare = is ']'

openCurly :: Parser Char
openCurly = is '{'

closeCurly :: Parser Char
closeCurly = is '}'

openAngle :: Parser Char
openAngle = is '<'

closeAngle :: Parser Char
closeAngle = is '>'
