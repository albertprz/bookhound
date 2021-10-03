module Converters.ToXml where

import Xml.Ast (XmlExpression(..), literalExpression)
import Json.Ast (JsExpression(..))
import qualified Data.Map as Map
import Data.Char (toLower)
import Json.Parser (json)
import ParserCombinators (Parser(parse))


class ToXml a where
  toXml :: a -> XmlExpression


instance ToXml JsExpression where

  toXml x = case x of
    JsNull       -> literalExpression "null"
    JsNumber n   -> literalExpression $ show n
    JsBool bool  -> literalExpression $ toLower <$> show bool
    JsString str -> literalExpression str

    JsArray arr  -> XmlExpression "array" Map.empty (elemExpr <$> arr) where

      elemExpr elem = XmlExpression { tagName = "elem",
                                       fields = Map.empty,
                                       expressions = [toXml elem] }

    JsObject obj -> XmlExpression "object" Map.empty (keyValueExpr <$> Map.toList obj)  where

      keyValueExpr (key, value) = XmlExpression { tagName = read $ show key,
                                                  fields = Map.empty,
                                                  expressions = [toXml value] }
