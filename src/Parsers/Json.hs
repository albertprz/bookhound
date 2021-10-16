{-# LANGUAGE PostfixOperators #-}

module Parsers.Json (json, nil, number, bool, string, array, object) where

import Parser(Parser(parse))
import ParserCombinators (IsMatch(..), (<|>), (>>>), (|*), (|?))
import Parsers.Number (double)
import Parsers.Collections (listOf, mapOf)
import Parsers.Char (char, doubleQuote)
import Parsers.String (withinDoubleQuotes, spacing, maybeWithin)
import SyntaxTrees.Json (JsExpression(..))


number :: Parser JsExpression
number = JsNumber <$> double


bool :: Parser JsExpression
bool = JsBool <$> (True  <$ is "true") <|>
                  (False <$ is "false")


string :: Parser JsExpression
string = JsString <$> text


array :: Parser JsExpression
array = JsArray <$> listOf json


object :: Parser JsExpression
object = JsObject <$> mapOf text json


nil :: Parser JsExpression
nil = JsNull <$ is "null"


json :: Parser JsExpression
json = maybeWithin spacing jsValue where

  jsValue = number <|> bool <|> string <|> array <|> object <|> nil


text :: Parser String
text = withinDoubleQuotes (inverse doubleQuote |*)
