module Operations.ToJson (ToJson(..)) where

import SyntaxTrees.Json (JsExpression(..))
import SyntaxTrees.Xml  (XmlExpression(..))
import SyntaxTrees.Yaml (YamlExpression(..))
import SyntaxTrees.Toml  (TomlExpression(..))
import Parsers.Json (json)
import Parsers.String (spacing)
import Parser (runParser)
import ParserCombinators (IsMatch(..), (<|>), (|*), maybeWithin)

import qualified Data.Map as Map
import Data.Map (Map, elems)
import Data.Either (fromRight)


class ToJson a where
  toJson :: a -> JsExpression


instance {-# OVERLAPPABLE #-} ToJson JsExpression where
  toJson = id

instance ToJson XmlExpression where

  toJson XmlExpression { tagName = tag, expressions = exprs, .. }
    | tag == "literal"  = fromRight JsNull . runParser literalParser .
                              head . elems $ fields
    | tag == "array"    = JsArray $ childExprToJson <$> exprs
    | tag == "object"   = JsObject . Map.fromList $ (\x -> (tagName x, childExprToJson x)) <$>
                                        exprs
    | otherwise          = JsNull   where

        literalParser = json <|> (JsString <$> maybeWithin spacing (isNot '<' |*))

        childExprToJson = toJson . head . (expressions)


instance ToJson YamlExpression where

  toJson = \case
    YamlNull              -> JsNull
    YamlInteger n         -> JsNumber $ fromIntegral n
    YamlFloat n           -> JsNumber n
    YamlBool bool         -> JsBool bool
    YamlString str        -> JsString str
    YamlDate date         -> JsString $ show date
    YamlTime time         -> JsString $ show time
    YamlDateTime dateTime -> JsString $ show dateTime
    YamlList _ list       -> JsArray $ toJson <$> list
    YamlMap  _ mapping    -> JsObject $ toJson <$> mapping


instance ToJson TomlExpression where

  toJson = \case
    TomlNull              -> JsNull
    TomlInteger n         -> JsNumber $ fromIntegral n
    TomlFloat n           -> JsNumber n
    TomlBool bool         -> JsBool bool
    TomlString str        -> JsString str
    TomlDate date         -> JsString $ show date
    TomlTime time         -> JsString $ show time
    TomlDateTime dateTime -> JsString $ show dateTime
    TomlArray list        -> JsArray $ toJson <$> list
    TomlTable _ mapping   -> JsObject $ toJson <$> mapping



instance ToJson String where
  toJson = JsString

instance ToJson Char where
  toJson = JsString . pure

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

instance ToJson a => ToJson (Maybe a) where
  toJson = maybe JsNull toJson
