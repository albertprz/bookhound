module Parsers.Json (json, nil, number, bool, string, array, object) where

import Parser              (Parser, withError)
import ParserCombinators   (IsMatch (..), maybeWithin, (<|>), (|*))
import Parsers.Char        (colon, doubleQuote)
import Parsers.Collections (listOf, mapOf)
import Parsers.Number      (double)
import Parsers.String      (spacing, withinDoubleQuotes)
import SyntaxTrees.Json    (JsExpression (..))


json :: Parser JsExpression
json = maybeWithin spacing jsValue
  where
    jsValue = element <|> container


nil :: Parser JsExpression
nil = withError "Json Null"
  $ JsNull <$ is "null"

number :: Parser JsExpression
number = withError "Json Number"
  $ JsNumber <$> double


bool :: Parser JsExpression
bool = withError "Json Bool"
  $ JsBool <$> (True  <$ is "true" <|>
                False <$ is "false")


string :: Parser JsExpression
string = withError "Json String"
  $ JsString <$> text


array :: Parser JsExpression
array = withError "Json Array"
  $ JsArray <$> listOf json


object :: Parser JsExpression
object = withError "Json Object"
  $ JsObject <$> mapOf colon text json



element :: Parser JsExpression
element = number <|> bool <|> nil <|> string

container :: Parser JsExpression
container = array <|> object



text :: Parser String
text = withinDoubleQuotes (inverse doubleQuote |*)
