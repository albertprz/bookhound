{-# LANGUAGE UndecidableInstances #-}

module Operations.ToYaml (ToYaml(..)) where

import SyntaxTrees.Yaml  (YamlExpression(..), CollectionType(..))
import SyntaxTrees.Json (JsExpression(..))
import Operations.ToJson (ToJson(..))
import Parsers.Number (intLike)
import Parser (runParser)


class ToYaml a where
  toYaml :: a -> YamlExpression

instance {-# OVERLAPPABLE #-} ToJson a => ToYaml a where
  toYaml = toYaml . toJson

instance ToYaml YamlExpression where
  toYaml = id

instance ToYaml JsExpression where

  toYaml = \case
    JsNull       -> YamlNull
    JsNumber n   -> either (const (YamlFloat n)) YamlInteger $ runParser intLike $ show n
    JsBool bool  -> YamlBool bool
    JsString str -> YamlString str
    JsArray arr  -> YamlList Standard $ toYaml <$> arr
    JsObject obj -> YamlMap  Standard $ toYaml <$> obj
