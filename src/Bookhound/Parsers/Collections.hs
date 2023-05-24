module Bookhound.Parsers.Collections (collOf, listOf, tupleOf, mapOf) where

import Bookhound.Parser            (Parser, withErrorN)
import Bookhound.ParserCombinators (anySepBy, maybeWithin, satisfies)
import Bookhound.Parsers.Char      (closeCurly, closeParens, closeSquare, comma,
                                    openCurly, openParens, openSquare)
import Bookhound.Parsers.String    (spacing)

import           Data.Map (Map)
import qualified Data.Map as Map


collOf :: Parser a -> Parser b -> Parser c -> Parser d -> Parser [d]
collOf start end sep elemParser =
  start *> elemsParser <* end
    where
      elemsParser = anySepBy sep $ maybeWithin spacing elemParser


listOf :: Parser a -> Parser [a]
listOf = withErrorN (-1) "List"
  . collOf openSquare closeSquare comma


tupleOf :: Parser a -> Parser [a]
tupleOf = withErrorN (-1) "Tuple"
  . satisfies ((>= 2) . length)
  . collOf openParens closeParens comma


mapOf :: Ord b => Parser a -> Parser b -> Parser c -> Parser (Map b c)
mapOf sep p1 p2 = withErrorN (-1) "Map" $
  Map.fromList <$> collOf openCurly closeCurly comma mapEntry
    where
      mapEntry = (,) <$> p1 <* maybeWithin spacing sep <*> p2
