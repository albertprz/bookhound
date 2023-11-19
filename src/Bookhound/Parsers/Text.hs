module Bookhound.Parsers.Text where

import           Bookhound.Parser            (Parser, anyChar)
import           Bookhound.ParserCombinators (IsMatch (..), between,
                                              maybeBetween, maybeSurroundedBy,
                                              surroundedBy, (->>-), (|+), (|?),
                                              (||*), (||+))
import           Bookhound.Parsers.Char      (alpha, alphaNum, closeAngle,
                                              closeCurly, closeParens,
                                              closeSquare, digit, doubleQuote,
                                              letter, lower, newLine, openAngle,
                                              openCurly, openParens, openSquare,
                                              quote, space, spaceOrTab, tab,
                                              upper, whiteSpace)
import           Data.Text                   (Text)
import qualified Data.Text                   as Text


string :: Parser Text
string = (anyChar ||*)

word :: Parser Text
word = (inverse whiteSpace ||+)

digits :: Parser Text
digits = (digit ||+)

uppers :: Parser Text
uppers = (upper ||+)

lowers :: Parser Text
lowers = (lower ||+)

letters :: Parser Text
letters = (letter ||+)

alphas :: Parser Text
alphas = (alpha ||+)

alphaNums :: Parser Text
alphaNums = (alphaNum ||+)



spaces :: Parser Text
spaces = (space ||+)

tabs :: Parser Text
tabs = (tab ||+)

newLines :: Parser Text
newLines = (newLine ||+)

spacesOrTabs :: Parser Text
spacesOrTabs = (spaceOrTab ||+)

spacing :: Parser Text
spacing = (whiteSpace ||+)

blankLine :: Parser Text
blankLine = (spacesOrTabs |?) ->>- newLine

blankLines :: Parser Text
blankLines = fmap Text.concat (blankLine |+)



betweenQuotes :: Parser b -> Parser b
betweenQuotes = between quote

betweenDoubleQuotes :: Parser b -> Parser b
betweenDoubleQuotes = between doubleQuote

betweenParens :: Parser b -> Parser b
betweenParens = surroundedBy openParens closeParens

betweenSquareBrackets :: Parser b -> Parser b
betweenSquareBrackets = surroundedBy openSquare closeSquare

betweenCurlyBrackets :: Parser b -> Parser b
betweenCurlyBrackets = surroundedBy openCurly closeCurly

betweenAngleBrackets :: Parser b -> Parser b
betweenAngleBrackets = surroundedBy openAngle closeAngle



maybeBetweenQuotes :: Parser b -> Parser b
maybeBetweenQuotes = maybeBetween quote

maybeBetweenDoubleQuotes :: Parser b -> Parser b
maybeBetweenDoubleQuotes = maybeBetween doubleQuote

maybeBetweenParens :: Parser b -> Parser b
maybeBetweenParens = maybeSurroundedBy openParens closeParens

maybeBetweenSquareBrackets :: Parser b -> Parser b
maybeBetweenSquareBrackets = maybeSurroundedBy openSquare closeSquare

maybeBetweenCurlyBrackets :: Parser b -> Parser b
maybeBetweenCurlyBrackets = maybeSurroundedBy openCurly closeCurly

maybeBetweenAngleBrackets :: Parser b -> Parser b
maybeBetweenAngleBrackets = maybeSurroundedBy openAngle closeAngle
