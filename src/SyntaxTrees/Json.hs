module SyntaxTrees.Json (JsExpression(..)) where

import Utils.Foldable (stringify)
import Utils.Map (showMap)

import Data.Map (Map)
import Data.Char (toLower)



data JsExpression = JsNumber Double | JsBool Bool |
                    JsString String |
                    JsArray [JsExpression] |
                    JsObject (Map String JsExpression) |
                    JsNull deriving (Eq, Ord)


instance Show JsExpression where
  show = \case
    JsNull       -> "null"
    JsNumber n   -> show n
    JsBool bool  -> toLower <$> show bool
    JsString str -> show str
    JsArray arr  -> stringify ",\n" "[\n" "\n]" 2 $ show   <$> arr
    JsObject obj -> stringify ",\n" "{\n" "\n}" 2 $ showMap ": " id show obj
