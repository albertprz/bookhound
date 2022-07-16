module Parsers.Collections (collOf, listOf, tupleOf, mapOf) where

import Parser            (Parser, withError)
import ParserCombinators (anySepBy, maybeWithin, satisfies)
import Parsers.Char      (closeCurly, closeParens, closeSquare, comma,
                          openCurly, openParens, openSquare)
import Parsers.String    (spacing)

import           Data.Map (Map)
import qualified Data.Map as Map


collOf :: Parser a -> Parser b -> Parser c -> Parser d -> Parser [d]
collOf start end sep elemParser = withError "Collection"
  $ start *> elemsParser <* end
  where
    elemsParser = anySepBy sep $ maybeWithin spacing elemParser

listOf :: Parser a -> Parser [a]
listOf = withError "List" . collOf openSquare closeSquare comma

tupleOf :: Parser a -> Parser [a]
tupleOf = withError "Tuple" . satisfies ((>= 2) . length)
  . collOf openParens closeParens comma

mapOf :: Ord b => Parser a -> Parser b -> Parser c -> Parser (Map b c)
mapOf sep p1 p2 = withError "Map"
  $ Map.fromList <$> collOf openCurly closeCurly comma mapEntry
  where
    mapEntry = (,) <$> p1 <* maybeWithin spacing sep <*> p2
