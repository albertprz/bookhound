{-# LANGUAGE PostfixOperators #-}

module Parsers.String where

import Parser (Parser)
import ParserCombinators (IsMatch(..), (|*), (|+), (|?), (<|>), (>>>), within, withinBoth)
import Parsers.Char


string :: Parser String
string = (char |*)

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
blankLine = (spacesOrTabs |?) >>> newLine

blankLines :: Parser String
blankLines = mconcat <$> (blankLine |+)



withinQuotes :: Parser b -> Parser b
withinQuotes = within quote

withinDoubleQuotes :: Parser b -> Parser b
withinDoubleQuotes = within doubleQuote

withinParens :: Parser b -> Parser b
withinParens = withinBoth openParens closeParens

withinBrackets :: Parser b -> Parser b
withinBrackets = withinBoth openBracket closeBracket

withinCurlyBrackets :: Parser b -> Parser b
withinCurlyBrackets = withinBoth openCurly closeCurly

withinAngleBrackets :: Parser b -> Parser b
withinAngleBrackets = withinBoth openAngle closeAngle
