{-# LANGUAGE PostfixOperators, FlexibleInstances, IncoherentInstances #-}

module Converters.ToJson where

import SyntaxTrees.Xml  (XmlExpression(..))
import SyntaxTrees.Json (JsExpression(..))
import Parsers.Json (json)
import Parsers.String (maybeWithinSpacing)
import Parser (Parser(parse), toEither)
import ParserCombinators (IsMatch(..), (<|>), (|*))

import qualified Data.Map as Map
import Data.Map (Map, elems, mapKeys)
import Data.Either (fromLeft)


class ToJson a where
  toJson :: a -> JsExpression


instance ToJson JsExpression where
  toJson = id


instance ToJson XmlExpression where

  toJson XmlExpression { tagName = tag, fields = flds, expressions = exprs }
    | tag == "literal"   = fromLeft JsNull . toEither . parse literalParser . head . elems $ flds
    | tag == "array"     = JsArray $ childExprToJson <$> exprs
    | tag == "object"    = JsObject . Map.fromList $ (\x -> (tagName x, childExprToJson x)) <$> exprs
    | otherwise          = JsNull   where

        literalParser = json <|> (JsString <$> maybeWithinSpacing (isNot '<' |*))

        childExprToJson = toJson . head . expressions



instance ToJson String where
  toJson = JsString

instance ToJson Char where
  toJson = JsString . (: [])

instance ToJson Integer where
  toJson = JsNumber . fromIntegral

instance ToJson Int where
  toJson = JsNumber . fromIntegral

instance ToJson Double where
  toJson = JsNumber

instance ToJson Bool where
  toJson = JsBool


instance ToJson a => ToJson [a] where
  toJson = JsArray . fmap toJson

instance ToJson a => ToJson (Map String a) where
  toJson = JsObject . fmap toJson
