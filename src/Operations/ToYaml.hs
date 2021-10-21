{-# LANGUAGE FlexibleInstances, UndecidableInstances, IncoherentInstances #-}

module Operations.ToYaml where

import SyntaxTrees.Yaml  (YamlExpression(..), CollectionType(..))
import SyntaxTrees.Json (JsExpression(..))
import Operations.ToJson (ToJson(..))
import Parsers.String (spacing)
import Parsers.Number (intLike)
import ParserCombinators ((<|>))
import Parser (Parser(), runParser)

import Data.Char (toLower)


class ToYaml a where
  toYaml :: a -> YamlExpression


instance ToYaml YamlExpression where
  toYaml = id


instance ToYaml JsExpression where

  toYaml x = case x of
    JsNull       -> YamlNull
    JsNumber n   -> either (const (YamlFloat n)) YamlInteger $ runParser intLike $ show n
    JsBool bool  -> YamlBool bool
    JsString str -> YamlString str
    JsArray arr  -> YamlList Standard $ toYaml <$> arr
    JsObject obj -> YamlMap  Standard $ toYaml <$> obj


instance ToJson a => ToYaml a where
  toYaml = toYaml . toJson
