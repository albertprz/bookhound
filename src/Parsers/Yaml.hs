{-# LANGUAGE PostfixOperators #-}

module Parsers.Yaml (yaml, nil, integer, float, bool, string, list, mapping) where


import Parser(Parser(..), ParseError(..), errorParser, check, andThen, exactly)
import ParserCombinators (IsMatch(..), (<|>), (<#>), (|?), (|*), (|+), maybeWithin)
import Parsers.Number (double, hexInt, int, octInt)
import Parsers.String (blankLines, spacesOrTabs, blankLine)
import Parsers.Char (colon, dash, space, whiteSpace, newLine, question, dot, hashTag)
import SyntaxTrees.Yaml (YamlExpression(..))
import Normalizers.Yaml (text, normalize, indentationCheck)
import Parsers.Collections (mapOf, listOf)
import qualified Parsers.DateTime as Dt

import qualified Data.Map as Map
import Data.Map (Map)
import Data.List (nub)



nil :: Parser YamlExpression
nil = YamlNull <$ oneOf ["null", "Null", "NULL"]

integer :: Parser YamlExpression
integer = YamlInteger <$> (hexInt <|> octInt <|> int)

float :: Parser YamlExpression
float = YamlFloat <$> double

bool :: Parser YamlExpression
bool = YamlBool <$> (True  <$ oneOf ["true", "True", "TRUE"])    <|>
                    (False <$ oneOf ["false", "False", "FALSE"])


dateTime :: Parser YamlExpression
dateTime = YamlDateTime <$> Dt.dateTime

date :: Parser YamlExpression
date = YamlDate <$> Dt.date

time :: Parser YamlExpression
time = YamlTime <$> Dt.time


string :: Int -> Parser YamlExpression
string indent = YamlString <$> text indent


sequential :: Parser a -> Int -> Parser [YamlExpression]
sequential sep indent = listParser where

  listParser = indentationCheck elemParser indent

  elemParser = do n <- length <$> (space |*)
                  sep *> whiteSpace
                  elem <- yamlWithIndent n
                  pure (n, elem)


list :: Int -> Parser YamlExpression
list indent = YamlList <$> (jsonList <|> sequential dash indent)  where

  jsonList = maybeWithin spacesOrTabs $ listOf $ yamlWithIndent (-1)


set :: Int -> Parser YamlExpression
set indent = YamlList . nub <$> sequential question indent


mapping :: Int -> Parser YamlExpression
mapping indent = YamlMap <$> (jsonMap <|> yamlMap)  where

  yamlMap = Map.fromList <$> mapParser
  jsonMap = maybeWithin spacesOrTabs $ mapOf colon (text 100) $ yamlWithIndent (-1)

  mapParser = indentationCheck keyValueParser indent

  keyValueParser = do n <- length <$> (space |*)
                      key <- show <$> element indent
                      (spacesOrTabs |?)
                      colon *> whiteSpace
                      value <- yamlWithIndent n
                      pure (n, (key, value))



element :: Int -> Parser YamlExpression
element indent = exactly (dateTime <|> date <|> time <|> float <|>
                          integer <|> bool <|> nil) <|>
                    string indent

container :: Int -> Parser YamlExpression
container indent = list indent <|> mapping indent <|> set indent



yamlWithIndent :: Int -> Parser YamlExpression
yamlWithIndent indent = maybeWithin ((blankLine <|> comment <|> directive <|>
                                      docStart <|> docEnd) |+)
                        yamlValue  where

  yamlValue = container indent <|> maybeWithin spacesOrTabs (element indent)

  comment = hashTag *> (inverse newLine |+) <* newLine

  directive = is "%" *> (inverse space *> (inverse newLine |+)) <* newLine

  docStart = dash <#> 3
  docEnd = dot <#> 3



yaml :: Parser YamlExpression
yaml =  normalize `andThen` yamlWithIndent (-1)
