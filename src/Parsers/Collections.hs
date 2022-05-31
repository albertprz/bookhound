module Parsers.Collections (collOf, listOf, tupleOf, mapOf) where

import Parser (Parser)
import ParserCombinators (maybeWithin, anySepBy, satisfies)
import Parsers.Char (comma, openSquare, closeSquare, openParens,
                     closeParens, openCurly, closeCurly)
import Parsers.String(spacing)

import qualified Data.Map as Map
import Data.Map(Map)


collOf :: Parser a -> Parser b -> Parser c -> Parser d -> Parser [d]
collOf start end sep elemParser = start *> elemsParser <* end   where

  elemsParser = anySepBy sep $ maybeWithin spacing elemParser

listOf :: Parser a -> Parser [a]
listOf = collOf openSquare closeSquare comma

tupleOf :: Parser a -> Parser [a]
tupleOf = satisfies ((>= 2) . length) . collOf openParens closeParens comma

mapOf :: Ord b => Parser a -> Parser b -> Parser c -> Parser (Map b c)
mapOf sep p1 p2 = Map.fromList <$> collOf openCurly closeCurly comma mapEntry  where

  mapEntry = (,) <$> p1 <* maybeWithin spacing sep <*> p2
