module Bookhound.SyntaxTrees.Json (JsExpression(..)) where

import Bookhound.Internal.Foldable (stringify)
import Bookhound.Internal.Map      (showMap)

import Data.Char (toLower)
import Data.Map  (Map)



data JsExpression
  = JsNumber Double
  | JsBool Bool
  | JsString String
  | JsArray [JsExpression]
  | JsObject (Map String JsExpression)
  | JsNull
  deriving (Eq, Ord)


instance Show JsExpression where
  show = \case
    JsNull       -> "null"
    JsNumber n   -> show n
    JsBool bool  -> toLower <$> show bool
    JsString str -> show str
    JsArray arr  -> stringify ",\n" "[\n" "\n]" 2 $ show   <$> arr
    JsObject obj -> stringify ",\n" "{\n" "\n}" 2 $ showMap ": " id show obj
