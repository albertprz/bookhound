{-# LANGUAGE PostfixOperators #-}

module Parsers.Collections where

import ParserCombinators (Parser, IsMatch(..), (<|>), (|*), (|?))
import Parsers.Char (colon)
import Parsers.String(spaces)

import Data.Foldable (Foldable(toList))
import qualified Data.Map as Map


collOf :: Char -> Char -> Char -> Parser a -> Parser [a]
collOf sep start end parser = do is start
                                 elems <- (elemParser |*)
                                 spaces
                                 elem <- (parser |?)
                                 spaces
                                 is end
                                 pure (elems ++ toList elem) where

  elemParser = do spaces
                  elem <- parser
                  spaces
                  is sep
                  pure elem


listOf :: Parser a -> Parser [a]
listOf = collOf ',' '[' ']'

tupleOf :: Parser a -> Parser [a]
tupleOf = collOf ',' '(' ')'

mapOf :: Ord a => Parser a -> Parser b -> Parser (Map.Map a b)
mapOf p1 p2 = Map.fromList <$> collOf ',' '{' '}' (mapEntryOf p1 p2) where

  mapEntryOf p1 p2 = do key <- p1
                        spaces
                        colon
                        spaces
                        value <- p2
                        pure (key, value)
