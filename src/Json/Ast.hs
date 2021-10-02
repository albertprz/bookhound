{-# LANGUAGE PostfixOperators #-}

module Json.Ast where

import Util.FoldableOps (stringify)

import Data.Map (Map, keys, elems)
import Data.Ord as Ord ( Ordering(EQ) )
import Data.Char (toLower)

data JsExpression = JsNumber Double | JsBool Bool |
                    JsString String | JsArray [JsExpression] |
                    JsObject (Map JsExpression JsExpression) |
                    JsNull


instance Eq JsExpression where
  (==) a b = case (a, b) of
    (JsNumber x, JsNumber y) -> x == y
    (JsBool x, JsBool y)     -> x == y
    (JsString x, JsString y) -> x == y
    (JsArray x, JsArray y)   -> x == y
    (JsObject x, JsObject y) -> x == y
    (JsNull, JsNull)         -> True
    (_, _ )                  -> False



instance Ord JsExpression where
  compare a b = case (a, b) of
    (JsNumber x, JsNumber y) -> compare x y
    (JsBool x, JsBool y)     -> compare x y
    (JsString x, JsString y) -> compare x y
    (JsArray x, JsArray y)   -> compare x y
    (JsObject x, JsObject y) -> compare x y
    (JsNull, JsNull)         -> Ord.EQ
    (_, _)                   -> error "cannot compare these values"


instance Show JsExpression where
  show expr = case expr of
    JsNull       -> "null"
    JsNumber x   -> show x
    JsBool x     -> toLower <$> show x
    JsString x   -> show x
    JsArray arr  -> stringify "," "[" "]" $ show   <$> arr
    JsObject obj -> stringify "," "{" "}" $ showFn <$> tuples where

      showFn (x, y) = show x ++ ": " ++ show y
      tuples = zip (keys obj) (elems obj)
