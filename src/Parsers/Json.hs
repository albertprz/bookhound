{-# LANGUAGE PostfixOperators #-}

module Parsers.Json (json) where

import Parser(Parser)
import ParserCombinators (IsMatch(..), (<|>), (>>>), (|*), (|?))
import Parsers.Number (double)
import Parsers.Collections (listOf, mapOf)
import Parsers.Char (char, alphaNum, doubleQuote)
import Parsers.String (withinDoubleQuotes, spacing, maybeWithin)
import SyntaxTrees.Json (JsExpression(..))


number :: Parser JsExpression
number = JsNumber <$> double


bool :: Parser JsExpression
bool = JsBool <$> (True  <$ is "true") <|>
                  (False <$ is "false")


string :: Parser JsExpression
string = JsString <$> withinDoubleQuotes (inverse doubleQuote |*)


array :: Parser JsExpression
array = JsArray <$> listOf json


object :: Parser JsExpression
object = JsObject <$> mapOf (withinDoubleQuotes (inverse doubleQuote |*)) json


nil :: Parser JsExpression
nil = JsNull <$ is "null"


json :: Parser JsExpression
json = maybeWithin spacing jsValue where

  jsValue = number <|> bool <|> string <|> array <|> object <|> nil
