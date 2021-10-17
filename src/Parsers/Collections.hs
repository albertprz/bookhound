{-# LANGUAGE PostfixOperators #-}

module Parsers.Collections where

import Parser (Parser)
import ParserCombinators (IsMatch(..), (<|>), (|*), (|?), maybeWithin)
import Parsers.Char (colon)
import Parsers.String(spacing)

import qualified Data.Foldable as Foldable
import qualified Data.Map as Map
import Data.Map(Map)


collOf :: Char -> Char -> Char -> Parser a -> Parser [a]
collOf sep start end parser = do is start
                                 elems <- ((maybeWithin spacing $ parser <* is sep) |*)
                                 elem  <- maybeWithin spacing (parser |?)
                                 is end
                                 pure (elems ++ Foldable.toList elem)


listOf :: Parser a -> Parser [a]
listOf = collOf ',' '[' ']'

tupleOf :: Parser a -> Parser [a]
tupleOf = collOf ',' '(' ')'

mapOf :: Ord a => Parser a -> Parser b -> Parser (Map a b)
mapOf p1 p2 = Map.fromList <$> collOf ',' '{' '}' (mapEntryOf p1 p2) where

  mapEntryOf p1 p2 = do key <- p1
                        maybeWithin spacing colon
                        value <- p2
                        pure (key, value)
