{-# LANGUAGE PostfixOperators #-}

module Parsers.Json (json, nil, number, bool, string, array, object) where

import Parser(Parser(parse), exactly)
import ParserCombinators (IsMatch(..), (<|>), (>>>), (|*), (|?), maybeWithin)
import Parsers.Number (double)
import Parsers.Collections (listOf, mapOf)
import Parsers.Char (char, doubleQuote, colon)
import Parsers.String (withinDoubleQuotes, spacing)
import SyntaxTrees.Json (JsExpression(..))



nil :: Parser JsExpression
nil = JsNull <$ is "null"

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
object = JsObject <$> mapOf colon text json


element :: Parser JsExpression
element = exactly number <|> exactly bool <|> exactly nil <|> exactly string

container :: Parser JsExpression
container = array <|> object


json :: Parser JsExpression
json = maybeWithin spacing jsValue where

  jsValue = element <|> container


text :: Parser String
text = withinDoubleQuotes (inverse doubleQuote |*)
