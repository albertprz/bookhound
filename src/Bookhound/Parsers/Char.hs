module Bookhound.Parsers.Char (module AnyChar, digit, hexDigit, octDigit, upper, lower, alpha, alphaNum, space, tab, newLine, spaceOrTab, whiteSpace, comma, dot, colon, quote, doubleQuote, dash, plus, equal, underscore, hashTag, question, openParens, closeParens, openSquare, closeSquare, openCurly, closeCurly, openAngle, closeAngle)  where

import qualified Bookhound.Parser            as AnyChar (anyChar)
import qualified Bookhound.Parser            as Parser
import           Bookhound.ParserCombinators (IsMatch (..))

import Bookhound.Parser    (Parser, anyChar)
import Control.Applicative
import Data.Char           (isAsciiLower, isAsciiUpper, isDigit, isHexDigit,
                            isOctDigit)

digit :: Parser Char
digit = satisfy isDigit

hexDigit :: Parser Char
hexDigit = satisfy isHexDigit

octDigit :: Parser Char
octDigit = satisfy isOctDigit

upper :: Parser Char
upper = satisfy isAsciiUpper

lower :: Parser Char
lower = satisfy isAsciiLower

alpha :: Parser Char
alpha = satisfy isAlpha

alphaNum :: Parser Char
alphaNum = satisfy isAlphaNum


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


isAlpha :: Char -> Bool
isAlpha x = isAsciiLower x || isAsciiUpper x

isAlphaNum :: Char -> Bool
isAlphaNum x = isAlpha x || isDigit x

satisfy :: (Char -> Bool) -> Parser Char
satisfy = flip Parser.satisfy anyChar
