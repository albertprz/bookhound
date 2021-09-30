{-# LANGUAGE PostfixOperators, TypeFamilies #-}

module Parsers(module Parsers, Parser, char) where

import Internal.Parser(Parser(parse), char, ParseError (UnexpectedChar), errorParser, ParseResult)
import ParserCombinators (IsMatch(..), (<|>), (|+), (|?), (|*), (>>>), someTimes, maybeTimes)

import Data.Foldable (Foldable(toList))
import qualified Data.Map as Map



-- Char parsers
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



-- Number parsers
posInt :: Parser Integer
posInt = read <$> (digit |+)

negInt :: Parser Integer
negInt = read <$> dash >>> (digit |+)

int :: Parser Integer
int = negInt <|> posInt

number :: Parser Double
number = read <$> int >>> (dot >>> int |?)



-- Collection parsers
collOf :: Char -> Char -> Char -> Parser a -> Parser [a]
collOf sep start end parser = do is start
                                 elems <- (elemParser |*)
                                 spaces
                                 elem <- (parser |?)
                                 spaces
                                 is end
                                 pure (elems ++ toList elem) where

  elemParser = do spaces
                  elem <- parser
                  spaces
                  is sep
                  pure elem


listOf :: Parser a -> Parser [a]
listOf = collOf ',' '[' ']'

tupleOf :: Parser a -> Parser [a]
tupleOf = collOf ',' '(' ')'

mapOf :: Ord a => Parser a -> Parser b -> Parser (Map.Map a b)
mapOf p1 p2 = Map.fromList <$> collOf ',' '{' '}' (mapEntryOf p1 p2) where

  mapEntryOf p1 p2 = do key <- p1
                        spaces
                        colon
                        spaces
                        value <- p2
                        pure (key, value)


-- Symbol parsers
space :: Parser Char
space = is ' '

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



-- String parsers
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
