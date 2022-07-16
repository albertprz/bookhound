module Operations.Finder (Finder(..)) where


import Parser            (runParser)
import ParserCombinators (IsMatch (..), (<|>), (|*))
import Parsers.Char      (dot)
import Parsers.Number    (unsignedInt)
import Parsers.String    (withinSquareBrackets)
import SyntaxTrees.Json  (JsExpression (..))
import SyntaxTrees.Toml  (TomlExpression (..))
import SyntaxTrees.Yaml  (YamlExpression (..))


import Data.Either (fromRight)
import Data.Maybe  (listToMaybe)

import qualified Data.Map  as Map
import           Data.Text (pack)



class Finder a where

  toList :: a -> [(String, a)]
  findAll :: ((String, a) -> Bool) -> a -> [a]
  find :: ((String, a) -> Bool) -> a -> Maybe a
  findByKeys :: [String] -> a -> Maybe a
  findByPath :: String -> a -> Maybe a


  findAll f = fmap snd . filter f . toList
  find f = listToMaybe . findAll f

  findByKeys []       expr = Just expr
  findByKeys (x : xs) expr = findByKey x expr >>= findByKeys xs  where

    findByKey key = find (\(str, _) -> str == key)

  findByPath path = findByKeys pathSeq where

    pathSeq = fromRight [] $ runParser parsePath $ pack path
    parsePath = is '$' *> ((index <|> key) |*)

    index = show <$> withinSquareBrackets unsignedInt
    key   = dot *> word
    word  = (noneOf ['.', '['] |*)



instance Finder JsExpression where
  toList = \case
    nil@JsNull       -> [("", nil)]
    n@(JsNumber _)   -> [("", n)]
    bool@(JsBool _)  -> [("", bool)]
    str@(JsString _) -> [("", str)]
    JsArray arr      -> zip (show <$> [0 .. length arr - 1]) arr
    JsObject obj     -> Map.toList obj


instance Finder YamlExpression where
  toList = \case
    nil@YamlNull              -> [("", nil)]
    n@(YamlInteger _)         -> [("", n)]
    n@(YamlFloat _)           -> [("", n)]
    bool@(YamlBool _)         -> [("", bool)]
    str@(YamlString _)        -> [("", str)]
    date@(YamlDate _)         -> [("", date)]
    time@(YamlTime _)         -> [("", time)]
    dateTime@(YamlDateTime _) -> [("", dateTime)]
    YamlList _ arr            -> zip (show <$> [0 .. length arr - 1]) arr
    YamlMap _ obj             -> Map.toList obj


instance Finder TomlExpression where
  toList = \case
    nil@TomlNull              -> [("", nil)]
    n@(TomlInteger _)         -> [("", n)]
    n@(TomlFloat _)           -> [("", n)]
    bool@(TomlBool _)         -> [("", bool)]
    str@(TomlString _)        -> [("", str)]
    date@(TomlDate _)         -> [("", date)]
    time@(TomlTime _)         -> [("", time)]
    dateTime@(TomlDateTime _) -> [("", dateTime)]
    TomlArray arr             -> zip (show <$> [0 .. length arr - 1]) arr
    TomlTable _ obj           -> Map.toList obj
