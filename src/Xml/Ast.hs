module Xml.Ast where

import Util.FoldableOps (stringify)

import Data.Map (Map, keys, elems)
import Data.Ord as Ord ( Ordering(EQ) )
import Data.Char (toLower)


data XmlExpression = XmlExpression {
    tagName :: String
  , fields :: Map String String
  , expressions :: [XmlExpression]
  }
