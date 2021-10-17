module SyntaxTrees.Toml where

import Utils.DateTimeOps ()

import Data.Map (Map, keys, elems)
import qualified Data.Map as Map
import Data.Time (Day, TimeOfDay, ZonedTime)


data TomlExpression = TomlInteger Integer | TomlFloat Double | TomlBool Bool |
                      TomlString String | TomlDate Day |
                      TomlTime TimeOfDay | TomlDateTime ZonedTime |
                      TomlList [TomlExpression] |
                      TomlMap (Map String TomlExpression) |
                      TomlNull
                    deriving (Eq, Ord)
