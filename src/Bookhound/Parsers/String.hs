module Bookhound.Parsers.String where

import Bookhound.Parser            (Parser, anyChar)
import Bookhound.ParserCombinators (IsMatch (..), maybeWithin, maybeWithinBoth,
                                    within, withinBoth, (->>-), (|*), (|+),
                                    (|?))
import Bookhound.Parsers.Char      (alpha, alphaNum, closeAngle, closeCurly,
                                    closeParens, closeSquare, digit,
                                    doubleQuote, letter, lower, newLine,
                                    openAngle, openCurly, openParens,
                                    openSquare, quote, space, spaceOrTab, tab,
                                    upper, whiteSpace)


string :: Parser String
string = (anyChar |*)

word :: Parser String
word = (inverse whiteSpace |+)

digits :: Parser String
digits = (digit |+)

uppers :: Parser String
uppers = (upper |+)

lowers :: Parser String
lowers = (lower |+)

letters :: Parser String
letters = (letter |+)

alphas :: Parser String
alphas = (alpha |+)

alphaNums :: Parser String
alphaNums = (alphaNum |+)



spaces :: Parser String
spaces = (space |+)

tabs :: Parser String
tabs = (tab |+)

newLines :: Parser String
newLines = (newLine |+)

spacesOrTabs :: Parser String
spacesOrTabs = (spaceOrTab |+)

spacing :: Parser String
spacing = (whiteSpace |+)

blankLine :: Parser String
blankLine = (spacesOrTabs |?) ->>- newLine

blankLines :: Parser String
blankLines = mconcat <$> (blankLine |+)



withinQuotes :: Parser b -> Parser b
withinQuotes = within quote

withinDoubleQuotes :: Parser b -> Parser b
withinDoubleQuotes = within doubleQuote

withinParens :: Parser b -> Parser b
withinParens = withinBoth openParens closeParens

withinSquareBrackets :: Parser b -> Parser b
withinSquareBrackets = withinBoth openSquare closeSquare

withinCurlyBrackets :: Parser b -> Parser b
withinCurlyBrackets = withinBoth openCurly closeCurly

withinAngleBrackets :: Parser b -> Parser b
withinAngleBrackets = withinBoth openAngle closeAngle



maybeWithinQuotes :: Parser b -> Parser b
maybeWithinQuotes = maybeWithin quote

maybeWithinDoubleQuotes :: Parser b -> Parser b
maybeWithinDoubleQuotes = maybeWithin doubleQuote

maybeWithinParens :: Parser b -> Parser b
maybeWithinParens = maybeWithinBoth openParens closeParens

maybeWithinSquareBrackets :: Parser b -> Parser b
maybeWithinSquareBrackets = maybeWithinBoth openSquare closeSquare

maybeWithinCurlyBrackets :: Parser b -> Parser b
maybeWithinCurlyBrackets = maybeWithinBoth openCurly closeCurly

maybeWithinAngleBrackets :: Parser b -> Parser b
maybeWithinAngleBrackets = maybeWithinBoth openAngle closeAngle
