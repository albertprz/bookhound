module SyntaxTrees.Yaml where

import Utils.FoldableOps (stringify)

import Data.Map (Map, keys, elems)
import qualified Data.Map as Map

import Data.Char (toLower)

data YamlExpression = YamlInteger Integer | YamlFloat Double | YamlBool Bool |
                      YamlString String | YamlList [YamlExpression] |
                      YamlMap (Map String YamlExpression) | YamlNull
                    deriving (Eq, Ord)



instance Show YamlExpression where
  show expr = case expr of
    YamlNull        -> "null"
    YamlInteger n   -> show n
    YamlFloat n     -> show n
    YamlBool bool   -> toLower <$> show bool
    YamlString str  -> str
    YamlList list   -> stringify "\n" "\n" "" 2 $ ("- " ++) . show <$> list
    YamlMap mapping -> stringify "\n" "\n" "" 2 $ showFn           <$> tuples where

      showFn (x, y) = x ++ ": " ++ show y
      tuples = zip (keys mapping) (elems mapping)
