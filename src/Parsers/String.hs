{-# LANGUAGE PostfixOperators #-}

module Parsers.String where

import Parser (Parser)
import ParserCombinators (IsMatch(..), (|*), (|+), (|?), (<|>), (>>>))
import Parsers.Char
import Utils.MonadOps (extract)


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
blankLine = (spacesOrTabs |?) >>> whiteSpace

blankLines :: Parser String
blankLines = mconcat <$> (blankLine |+)



within :: Parser a -> Parser b -> Parser b
within p = extract p p

maybeWithin :: Parser a -> Parser b -> Parser b
maybeWithin p = within (p |?)

withinBoth :: Parser a -> Parser b -> Parser c -> Parser c
withinBoth = extract

maybeWithinBoth :: Parser a -> Parser b -> Parser c -> Parser c
maybeWithinBoth p1 p2 = extract (p1 |?) (p2 |?)



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
