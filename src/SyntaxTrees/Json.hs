{-# LANGUAGE PostfixOperators, TupleSections #-}

module SyntaxTrees.Json (JsExpression(..), findByPath, findByKeys, findAll) where

import Utils.FoldableOps (stringify)
import Utils.MapOps (showMap)

import Parser (parse, toEither)
import ParserCombinators (IsMatch(..), (|*), (<|>), (>>>))
import Parsers.String (withinSquareBrackets)
import Parsers.Char (dot)
import Parsers.Number (unsignedInt)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (listToMaybe, maybeToList)
import Data.Char (toLower)
import Data.Either (fromRight)



data JsExpression = JsNumber Double | JsBool Bool |
                    JsString String |
                    JsArray [JsExpression] |
                    JsObject (Map String JsExpression) |
                    JsNull deriving (Eq, Ord)


instance Show JsExpression where
  show expr = case expr of
    JsNull       -> "null"
    JsNumber n   -> show n
    JsBool bool  -> toLower <$> show bool
    JsString str -> show str
    JsArray arr  -> stringify ",\n" "[\n" "\n]" 2 $ show   <$> arr
    JsObject obj -> stringify ",\n" "{\n" "\n}" 2 $ showMap ": " id show obj



flatten :: JsExpression -> [(String, JsExpression)]
flatten expr = case expr of
  JsNull             -> []
  n @ (JsNumber _)   -> [("", n)]
  bool @ (JsBool _)  -> [("", bool)]
  str @ (JsString _) -> [("", str)]
  JsArray arr        -> zip (show <$> [0 .. length arr - 1]) arr
  JsObject obj       -> Map.toList obj


findAll :: ((String, JsExpression) -> Bool) -> JsExpression -> [JsExpression]
findAll f = fmap snd . filter f . flatten


find :: ((String, JsExpression) -> Bool) -> JsExpression -> Maybe JsExpression
find f = listToMaybe . findAll f


findByKeys :: [String] -> JsExpression -> Maybe JsExpression
findByKeys []       expr = Just expr
findByKeys (x : xs) expr = findByKey x expr >>= findByKeys xs where

  findByKey key = find (\(str, _) -> str == key)


findByPath :: String -> JsExpression -> Maybe JsExpression
findByPath path = findByKeys pathSeq where

  pathSeq = fromRight [] . toEither $ parse parseJsonPath path
  parseJsonPath = is '$' *> (index <|> key |*)

  index = show <$> withinSquareBrackets unsignedInt
  key   = dot *> word
  word  = (noneOf ['.', '['] |*)
