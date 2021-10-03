{-# LANGUAGE PostfixOperators #-}

module Json.Ast where

import Util.FoldableOps (stringify)

import Data.Map (Map, keys, elems)
import Data.Ord as Ord ( Ordering(EQ) )
import Data.Char (toLower)

data JsExpression = JsNumber Double | JsBool Bool |
                    JsString String | JsArray [JsExpression] |
                    JsObject (Map JsExpression JsExpression) |
                    JsNull deriving (Eq, Ord)


instance Show JsExpression where
  show expr = case expr of
    JsNull       -> "null"
    JsNumber n   -> show n
    JsBool bool  -> toLower <$> show bool
    JsString str -> show str
    JsArray arr  -> stringify ",\n" "[\n" "\n]" 2 $ show   <$> arr
    JsObject obj -> stringify ",\n" "{\n" "\n}" 2 $ showFn <$> tuples where

      showFn (x, y) = show x ++ ": " ++ show y
      tuples = zip (keys obj) (elems obj)
