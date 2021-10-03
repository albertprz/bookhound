module Xml.Ast where

import Util.FoldableOps (stringify)

import Data.Map (Map, keys, elems)
import Data.Ord as Ord ( Ordering(EQ) )
import Data.Char (toLower)
import Data.List (intercalate)


data XmlExpression = XmlExpression {
    tagName     :: String
  , fields      :: Map String String
  , expressions :: [XmlExpression]
  } deriving (Eq, Ord)


instance Show XmlExpression where
  show expr = if tagName expr == "literalValue" then literal else complexExpr where

    literal = head . elems $ fields expr

    complexExpr = "<" ++ tagName expr ++ showFields (fields expr) ++ exprs

    exprs = if null $ expressions expr then "/>"
            else                             ">"  ++ ending

    (sep, n) = if (tagName . head . expressions) expr == "literalValue" then ("", 0)
              else                                                           ("\n", 2)

    ending = stringify sep sep sep n (show <$> expressions expr) ++
            "</" ++ tagName expr ++ ">"


showFields :: Map String String -> String
showFields flds = if null flds then ""
                  else              " " ++ fieldsString where

      fieldsString = stringify " " "" "" 0 $ showFn <$> tuples
      showFn (x, y) = x ++ "=" ++ show y
      tuples = zip (keys flds) (elems flds)
