module SyntaxTrees.Yaml where

import Utils.FoldableOps (stringify)
import Utils.DateTimeOps ()

import Data.Map (Map, keys, elems)
import qualified Data.Map as Map
import Data.Char (toLower)
import Data.Time (Day, TimeOfDay, ZonedTime)



data YamlExpression = YamlInteger Integer | YamlFloat Double | YamlBool Bool |
                      YamlString String | YamlDate Day |
                      YamlTime TimeOfDay | YamlDateTime ZonedTime |
                      YamlList [YamlExpression] |
                      YamlMap (Map String YamlExpression) |
                      YamlNull
                    deriving (Eq, Ord)


instance Show YamlExpression where
  show expr = case expr of
    YamlNull              -> "null"
    YamlInteger n         -> show n
    YamlFloat n           -> show n
    YamlBool bool         -> toLower <$> show bool
    YamlDate date         -> show date
    YamlTime time         -> show time
    YamlDateTime dateTime -> show dateTime
    YamlString str        -> format str  where

      format str = (if (length (lines str) > 1) && not (any (`elem` str) forbiddenChar)
                    then "| \n"
                    else if length (lines str) > 1 then "\n"
                    else "") ++

                   (if not $ any (`elem` str) forbiddenChar  then str
                   else if '"' `elem` str  then "'"  ++ indented str 3 ++ "'"
                   else                         "\"" ++ indented str 3 ++ "\"")

      indented str n = head (lines str) ++
                       mconcat ((("\n" ++ replicate n ' ') ++) <$> tail (lines str))

      forbiddenChar = ['#', '&', '*', ',', '?', '-', ':', '[', ']', '{', '}'] ++
                      ['>', '|', ':', '!']

    YamlList list   -> stringify "\n" "\n" "" 2 $ ("- " ++) . show <$> list
    YamlMap mapping -> stringify "\n" "\n" "" 2 $ showFn           <$> tuples where

      showFn (x, y) = x ++ ": " ++ show y
      tuples = zip (keys mapping) (elems mapping)
