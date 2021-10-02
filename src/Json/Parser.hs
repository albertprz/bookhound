{-# LANGUAGE PostfixOperators #-}

module Json.Parser where

import ParserCombinators (Parser(parse), IsMatch(..), (<|>), (>>>), (|*), (|?))
import Parsers.Number (double)
import Parsers.Collections (listOf, mapOf)
import Parsers.Char (char, alphaNum)
import Parsers.String (withinDoubleQuotes, withinSpacing)
import Json.Ast (JsExpression(..))


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
object = JsObject <$> mapOf string json


nil :: Parser JsExpression
nil = JsNull <$ is "null"


json :: Parser JsExpression
json = withinSpacing jsValue where

  jsValue = number <|> bool <|> string <|> array <|> object <|> nil
