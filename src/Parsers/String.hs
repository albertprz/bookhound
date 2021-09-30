{-# LANGUAGE PostfixOperators #-}

module Parsers.String where

import ParserCombinators (Parser, IsMatch(..), (|*))
import Parsers.Char (char, digit, upper, lower, letter, alpha, alphaNum, space)


string :: Parser String
string = (char |*)

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
newLine = is "\n"

newLines :: Parser [String]
newLines = (newLine |*)
