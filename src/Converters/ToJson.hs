{-# LANGUAGE PostfixOperators, FlexibleInstances, IncoherentInstances #-}

module Converters.ToJson where

import SyntaxTrees.Json (JsExpression(..))
import SyntaxTrees.Xml  (XmlExpression(..))
import SyntaxTrees.Yaml (YamlExpression(..))
import Parsers.Json (json)
import Parsers.String (maybeWithin, spacing)
import Parser (Parser(parse), toEither)
import ParserCombinators (IsMatch(..), (<|>), (|*))

import qualified Data.Map as Map
import Data.Map (Map, elems, mapKeys)
import Data.Either (fromRight)


class ToJson a where
  toJson :: a -> JsExpression


instance ToJson JsExpression where
  toJson = id


instance ToJson XmlExpression where

  toJson XmlExpression { tagName = tag, fields = flds, expressions = exprs }
    | tag == "literal"   = fromRight JsNull . toEither . parse literalParser . head . elems $
      flds
    | tag == "array"     = JsArray $ childExprToJson <$> exprs
    | tag == "object"    = JsObject . Map.fromList $ (\x -> (tagName x, childExprToJson x)) <$>
      exprs
    | otherwise          = JsNull   where

        literalParser = json <|> (JsString <$> maybeWithin spacing (isNot '<' |*))

        childExprToJson = toJson . head . expressions


instance ToJson YamlExpression where

  toJson expr = case expr of
    YamlNull        -> JsNull
    YamlInteger n   -> JsNumber $ fromIntegral n
    YamlFloat n     -> JsNumber n
    YamlBool bool   -> JsBool bool
    YamlString str  -> JsString str
    YamlList list   -> JsArray $ toJson <$> list
    YamlMap mapping -> JsObject $ toJson <$> mapping


instance ToJson String where
  toJson = JsString

instance ToJson Char where
  toJson = JsString . (: [])

instance ToJson Int where
  toJson = JsNumber . fromIntegral

instance ToJson Integer where
  toJson = JsNumber . fromIntegral

instance ToJson Double where
  toJson = JsNumber

instance ToJson Bool where
  toJson = JsBool

instance ToJson a => ToJson [a] where
  toJson = JsArray . fmap toJson

instance ToJson a => ToJson (Map String a) where
  toJson = JsObject . fmap toJson
