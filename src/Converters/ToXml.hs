{-# LANGUAGE PostfixOperators, FlexibleInstances, UndecidableInstances, IncoherentInstances #-}

module Converters.ToXml where

import SyntaxTrees.Xml  (XmlExpression(..), literalExpression)
import SyntaxTrees.Json (JsExpression(..))
import Converters.ToJson (ToJson(..))

import qualified Data.Map as Map
import Data.Char (toLower)


class ToXml a where
  toXml :: a -> XmlExpression


instance ToXml XmlExpression where
  toXml = id


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

      keyValueExpr (key, value) = XmlExpression { tagName = key,
                                                  fields = Map.empty,
                                                  expressions = [toXml value] }


instance ToJson a => ToXml a where
  toXml = toXml . toJson
