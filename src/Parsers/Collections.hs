module Parsers.Collections (collOf, listOf, tupleOf, mapOf, csvOf) where

import Parser (Parser)
import ParserCombinators ((|*), (|?), maybeWithin)
import Parsers.Char (comma, openSquare, closeSquare, openParens,
                     closeParens, openCurly, closeCurly)
import Parsers.String(spacing)

import qualified Data.Foldable as Foldable
import qualified Data.Map as Map
import Data.Map(Map)


collOf :: Parser a -> Parser b -> Parser c -> Parser d -> Parser [d]
collOf sep start end elemParser = do start
                                     elems <- ((maybeWithin spacing $ elemParser <* sep) |*)
                                     element  <- maybeWithin spacing (elemParser |?)
                                     end
                                     pure (elems ++ Foldable.toList element)


listOf :: Parser a -> Parser [a]
listOf = collOf comma openSquare closeSquare

tupleOf :: Parser a -> Parser [a]
tupleOf = collOf comma openParens closeParens

mapOf :: Ord b => Parser a -> Parser b -> Parser c -> Parser (Map b c)
mapOf sep p1 p2 = Map.fromList <$> collOf comma openCurly closeCurly mapEntry  where

  mapEntry = do key <- p1
                maybeWithin spacing sep
                value <- p2
                pure (key, value)

csvOf :: Parser a -> Parser [a]
csvOf = collOf comma (pure ()) (pure ())
