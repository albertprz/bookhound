{-# LANGUAGE PostfixOperators #-}

module Parsers.Json (json) where

import Parser(Parser)
import ParserCombinators (IsMatch(..), (<|>), (>>>), (|*), (|?))
import Parsers.Number (double)
import Parsers.Collections (listOf, mapOf)
import Parsers.Char (char, alphaNum)
import Parsers.String (withinDoubleQuotes, maybeWithinSpacing)
import SyntaxTrees.Json (JsExpression(..))


number :: Parser JsExpression
number = JsNumber <$> double


bool :: Parser JsExpression
bool = JsBool <$> (True  <$ is "true") <|>
                  (False <$ is "false")


string :: Parser JsExpression
string = JsString <$> withinDoubleQuotes (isNot '"' |*)


array :: Parser JsExpression
array = JsArray <$> listOf json


object :: Parser JsExpression
object = JsObject <$> mapOf (withinDoubleQuotes (isNot '"' |*)) json


nil :: Parser JsExpression
nil = JsNull <$ is "null"


json :: Parser JsExpression
json = maybeWithinSpacing jsValue where

  jsValue = number <|> bool <|> string <|> array <|> object <|> nil
