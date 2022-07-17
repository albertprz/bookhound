{-# LANGUAGE UndecidableInstances #-}

module Bookhound.Operations.ToYaml (ToYaml(..)) where

import Bookhound.Operations.ToJson (ToJson (..))
import Bookhound.Parser            (runParser)
import Bookhound.Parsers.Number    (intLike)
import Bookhound.SyntaxTrees.Json  (JsExpression (..))
import Bookhound.SyntaxTrees.Yaml  (CollectionType (..), YamlExpression (..))
import Data.Text                   (pack)



class ToYaml a where
  toYaml :: a -> YamlExpression

instance {-# OVERLAPPABLE #-} ToJson a => ToYaml a where
  toYaml = toYaml . toJson

instance ToYaml YamlExpression where
  toYaml = id

instance ToYaml JsExpression where

  toYaml = \case
    JsNull       -> YamlNull
    JsNumber n   -> either (const (YamlFloat n)) YamlInteger $
      runParser intLike $ pack $ show n
    JsBool bool  -> YamlBool bool
    JsString str -> YamlString str
    JsArray arr  -> YamlList Standard $ toYaml <$> arr
    JsObject obj -> YamlMap  Standard $ toYaml <$> obj
