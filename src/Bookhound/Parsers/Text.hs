module Bookhound.Parsers.Text where

import           Bookhound.Parser            (Parser, anyChar)
import           Bookhound.ParserCombinators (IsMatch (..), between,
                                              maybeBetween, maybeSurroundedBy,
                                              surroundedBy, (->>-), (|+), (|?),
                                              (||*), (||+))
import           Bookhound.Parsers.Char      (alpha, alphaNum, closeAngle,
                                              closeCurly, closeParens,
                                              closeSquare, digit, doubleQuote,
                                              lower, newLine, openAngle,
                                              openCurly, openParens, openSquare,
                                              quote, space, spaceOrTab, tab,
                                              upper, whiteSpace)
import           Data.Text                   (Text)
import qualified Data.Text                   as Text


anyString :: Parser Text
anyString = (anyChar ||*)

word :: Parser Text
word = (inverse whiteSpace ||+)

digits :: Parser Text
digits = (digit ||+)

uppers :: Parser Text
uppers = (upper ||+)

lowers :: Parser Text
lowers = (lower ||+)

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
betweenQuotes = surroundedBy quote

betweenDoubleQuotes :: Parser b -> Parser b
betweenDoubleQuotes = surroundedBy doubleQuote

betweenSpacing :: Parser b -> Parser b
betweenSpacing = surroundedBy spacing

betweenParens :: Parser b -> Parser b
betweenParens = between openParens closeParens

betweenSquare :: Parser b -> Parser b
betweenSquare = between openSquare closeSquare

betweenCurlyBrackets :: Parser b -> Parser b
betweenCurlyBrackets = between openCurly closeCurly

betweenAngleBrackets :: Parser b -> Parser b
betweenAngleBrackets = between openAngle closeAngle



maybeBetweenQuotes :: Parser b -> Parser b
maybeBetweenQuotes = maybeSurroundedBy quote

maybeBetweenDoubleQuotes :: Parser b -> Parser b
maybeBetweenDoubleQuotes = maybeSurroundedBy doubleQuote

maybeBetweenSpacing :: Parser b -> Parser b
maybeBetweenSpacing = maybeSurroundedBy spacing

maybeBetweenParens :: Parser b -> Parser b
maybeBetweenParens = maybeBetween openParens closeParens

maybeBetweenSquareBrackets :: Parser b -> Parser b
maybeBetweenSquareBrackets = maybeBetween openSquare closeSquare

maybeBetweenCurlyBrackets :: Parser b -> Parser b
maybeBetweenCurlyBrackets = maybeBetween openCurly closeCurly

maybeBetweenAngleBrackets :: Parser b -> Parser b
maybeBetweenAngleBrackets = maybeBetween openAngle closeAngle
