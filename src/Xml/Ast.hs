{-# LANGUAGE NamedFieldPuns #-}

module Xml.Ast where

import Util.FoldableOps (stringify)

import Data.Map (Map, keys, elems)
import qualified Data.Map as Map

import Data.Maybe (listToMaybe)


data XmlExpression = XmlExpression {
    tagName     :: String
  , fields      :: Map String String
  , expressions :: [XmlExpression]
  } deriving (Eq, Ord)


instance Show XmlExpression where

  show XmlExpression { tagName = tag, fields, expressions }
    | tag == "|literal|"  = head . elems $ fields
    | otherwise           = "<" ++ tag ++ flds ++ innerExprs   where

        innerExprs = if null expressions then "/>"
                     else                     ">"  ++ ending

        (sep, n) = if (tagName . head) expressions == "|literal|" then ("", 0)
                   else                                                ("\n", 2)

        ending = stringify sep sep sep n (show <$> expressions) ++ "</" ++ tag ++ ">"

        flds | null fields = ""
             | otherwise = " " ++ fieldsString where

            fieldsString = stringify " " "" "" 0 $ showFn <$> tuples
            showFn (x, y) = x ++ "=" ++ show y
            tuples = zip (keys fields) (elems fields)



literalExpression :: String -> XmlExpression
literalExpression val = XmlExpression { tagName = "|literal|",
                                        fields = Map.fromList [("value", val)],
                                        expressions = [] }


flatten :: XmlExpression -> [XmlExpression]
flatten expr = expr : expressions expr >>= flatten


findAll :: (XmlExpression -> Bool) -> XmlExpression -> [XmlExpression]
findAll f = filter f . flatten


find :: (XmlExpression -> Bool) -> XmlExpression -> Maybe XmlExpression
find f = listToMaybe . findAll f
