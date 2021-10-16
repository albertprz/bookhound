{-# LANGUAGE PostfixOperators #-}

module Parsers.Yaml (yaml, nil, integer, float, bool, string, list, mapping) where


import Parser(Parser(..), ParseError(..), errorParser, check, andThen, exactly)
import ParserCombinators (IsMatch(..), (<|>), (|?), (|*), (|+))
import Parsers.Number (double, hexInt, int, octInt)
import Parsers.String (blankLines, maybeWithin, spacesOrTabs, blankLine)
import Parsers.Char (colon, dash, space, whiteSpace, newLine)
import SyntaxTrees.Yaml (YamlExpression(..))
import Normalizers.Yaml (text, normalize, indentationCheck)

import qualified Data.Map as Map
import Data.Map (Map)
import Parsers.Collections (mapOf, listOf)



nil :: Parser YamlExpression
nil = YamlNull <$ oneOf ["null", "Null", "NULL"]

integer :: Parser YamlExpression
integer = YamlInteger <$> (hexInt <|> octInt <|> int)

float :: Parser YamlExpression
float = YamlFloat <$> double

bool :: Parser YamlExpression
bool = YamlBool <$> (True  <$ oneOf ["true", "True", "TRUE"])    <|>
                    (False <$ oneOf ["false", "False", "FALSE"])


string :: Int -> Parser YamlExpression
string indent = YamlString <$> text indent


list :: Int -> Parser YamlExpression
list indent = YamlList <$> (jsonList <|> yamlList)  where

  yamlList = listParser
  jsonList = maybeWithin spacesOrTabs $ listOf $ yamlWithIndent (-1)

  listParser = indentationCheck elemParser indent

  elemParser = do n <- length <$> (space |*)
                  dash *> whiteSpace
                  elem <- yamlWithIndent n
                  pure (n, elem)


mapping :: Int -> Parser YamlExpression
mapping indent = YamlMap <$> (jsonMap <|> yamlMap)  where

  yamlMap = Map.fromList <$> mapParser
  jsonMap = maybeWithin spacesOrTabs $ mapOf (text 100) $ yamlWithIndent (-1)

  mapParser = indentationCheck keyValueParser indent

  keyValueParser = do n <- length <$> (space |*)
                      key <- text indent
                      (spacesOrTabs |?)
                      colon *> whiteSpace
                      value <- yamlWithIndent n
                      pure (n, (key, value))


start :: Parser String
start = is "---"

end :: Parser String
end = is "..."


yamlWithIndent :: Int -> Parser YamlExpression
yamlWithIndent indent = maybeWithin ((blankLine <|> start <|> end) |+) yamlValue  where

  yamlValue = yamlContainer <|> maybeWithin spacesOrTabs yamlElem

  yamlContainer = list indent <|> mapping indent

  yamlElem = (exactly float <|> exactly integer <|> exactly bool <|> exactly nil) <|>
                string indent



yaml :: Parser YamlExpression
yaml =  normalize `andThen` yamlWithIndent (-1)
