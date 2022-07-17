{-# LANGUAGE UndecidableInstances #-}

module Bookhound.Operations.ToXml (ToXml(..)) where

import Bookhound.Operations.ToJson (ToJson (..))
import Bookhound.SyntaxTrees.Json  (JsExpression (..))
import Bookhound.SyntaxTrees.Xml   (XmlExpression (..), literalExpression)

import           Data.Char (toLower)
import qualified Data.Map  as Map


class ToXml a where
  toXml :: a -> XmlExpression


instance {-# OVERLAPPABLE #-} ToJson a => ToXml a where
  toXml = toXml . toJson

instance ToXml XmlExpression where
  toXml = id


instance ToXml JsExpression where

  toXml = \case
    JsNull       -> literalExpression "null"
    JsNumber n   -> literalExpression $ show n
    JsBool bool  -> literalExpression $ toLower <$> show bool
    JsString str -> literalExpression str

    JsArray arr  -> XmlExpression "array" Map.empty (elemExpr <$> arr) where

      elemExpr element = XmlExpression { tagName = "elem",
                                       fields = Map.empty,
                                       expressions = [toXml element] }

    JsObject obj -> XmlExpression "object" Map.empty (keyValueExpr <$> Map.toList obj)  where

      keyValueExpr (key, value) = XmlExpression { tagName = key,
                                                  fields = Map.empty,
                                                  expressions = [toXml value] }
