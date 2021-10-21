module SyntaxTrees.Toml where

import Utils.DateTime (showDateTime)
import Utils.Foldable (stringify)
import Utils.Map (showMap)

import Data.Map (Map, keys, elems)
import qualified Data.Map as Map
import Data.Time (Day, TimeOfDay, ZonedTime(..), LocalTime(..))
import Data.Char (toLower)



data TomlExpression = TomlInteger Integer | TomlFloat Double | TomlBool Bool |
                      TomlString String | TomlDate Day |
                      TomlTime TimeOfDay | TomlDateTime ZonedTime |
                      TomlArray [TomlExpression] |
                      TomlTable TableType (Map String TomlExpression) |
                      TomlNull
                    deriving (Eq, Ord)


data TableType = TopLevel | Standard | Inline deriving (Eq, Ord)


instance Show TomlExpression where
  show expr = case expr of
    TomlNull                   -> "null"
    TomlInteger n              -> show n
    TomlFloat n                -> show n
    TomlBool bool              -> toLower <$> show bool
    TomlDate date              -> show date
    TomlTime time              -> show time
    TomlDateTime dateTime      -> showDateTime dateTime
    TomlString str             -> show str

    TomlTable Standard table   -> stringify "\n" "" "" 0 $ showMap " = " id show table

    TomlTable TopLevel table   -> stringify "\n\n" "\n" "\n" 0 $ showMap "" showTableHeader show table where
      showTableHeader header = if header /= "" then "[" ++ header ++ "]" ++ "\n"  else ""

    TomlTable Inline table     -> stringify (", " ++ sep) ("{ " ++ sep) (" }" ++ sep) n $
                                    showMap " = " id show table where
      (sep, n) = if (length . mconcat) (show <$> Map.toList table) >= 80 then ("\n", 2) else ("", 0)

    TomlArray arr              -> stringify (", " ++ sep) ("[ " ++ sep) (" ]" ++ sep) n $ show <$> arr where
      (sep, n) = if (length . mconcat) (show <$> arr) >= 80 then ("\n", 2) else ("", 0)
