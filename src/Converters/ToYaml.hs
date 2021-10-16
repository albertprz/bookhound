{-# LANGUAGE FlexibleInstances, UndecidableInstances, IncoherentInstances #-}

module Converters.ToYaml where

import SyntaxTrees.Yaml  (YamlExpression(..))
import SyntaxTrees.Json (JsExpression(..))
import Converters.ToJson (ToJson(..))

import qualified Data.Map as Map
import Data.Char (toLower)


class ToYaml a where
  toYaml :: a -> YamlExpression


instance ToYaml YamlExpression where
  toYaml = id


instance ToYaml JsExpression where

  toYaml x = case x of
    JsNull       -> YamlNull
    JsNumber n   -> YamlFloat n
    JsBool bool  -> YamlBool bool
    JsString str -> YamlString str
    JsArray arr  -> YamlList $ toYaml <$> arr
    JsObject obj -> YamlMap  $ toYaml <$> obj


instance ToJson a => ToYaml a where
  toYaml = toYaml . toJson