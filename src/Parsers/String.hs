{-# LANGUAGE PostfixOperators #-}

module Parsers.String where

import ParserCombinators (Parser, IsMatch(..), (|*), (|+), (|?), (<|>))
import Parsers.Char (char, digit, upper, lower, letter, alpha, alphaNum, space, quote, doubleQuote, openParens, closeParens, openBracket, closeBracket, openCurly, closeCurly, openAngle, closeAngle)
import Util.MonadOps (extract)


string :: Parser String
string = (char |*)

word :: Parser String
word = string `satisfies` notElem ' '

digits :: Parser String
digits = (digit |*)

uppers :: Parser String
uppers = (upper |*)

lowers :: Parser String
lowers = (lower |*)

letters :: Parser String
letters = (letter |*)

alphas :: Parser String
alphas = (alpha |*)

alphaNums :: Parser String
alphaNums = (alphaNum |*)



spaces :: Parser String
spaces = (space |*)

tab :: Parser String
tab = is "\t"

tabs :: Parser [String]
tabs = (tab |*)

newLine :: Parser String
newLine = oneOf ["\n", "\r"]

newLines :: Parser [String]
newLines = (newLine |*)

spacing :: Parser [String]
spacing = ((((: []) <$> space) <|> tab <|> newLine) |+)



withinQuotes :: Parser b -> Parser b
withinQuotes = extract quote quote

withinDoubleQuotes :: Parser b -> Parser b
withinDoubleQuotes = extract doubleQuote doubleQuote

withinParens :: Parser b -> Parser b
withinParens = extract openParens closeParens

withinBrackets :: Parser b -> Parser b
withinBrackets = extract openBracket closeBracket

withinCurlyBrackets :: Parser b -> Parser b
withinCurlyBrackets = extract openCurly closeCurly

withinAngleBrackets :: Parser b -> Parser b
withinAngleBrackets = extract openAngle closeAngle

withinSpacing :: Parser b -> Parser b
withinSpacing = extract spacing spacing

maybeWithinSpacing :: Parser b -> Parser b
maybeWithinSpacing = extract (spacing |?) (spacing |?)
