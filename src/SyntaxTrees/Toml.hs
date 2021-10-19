module SyntaxTrees.Toml where

import Utils.DateTimeOps (showDateTime)
import Utils.FoldableOps (stringify)
import Utils.MapOps (showMap)

import Data.Map (Map, keys, elems)
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
    TomlArray arr              -> stringify ", " "[ " " ]" 0 $ show <$> arr
    TomlTable Inline table     -> stringify ", " "{ " " }" 0 $ showMap " = " id show table
    TomlTable Standard table   -> stringify "\n" "" "" 0 $ showMap " = " id show table
    TomlTable TopLevel table   -> stringify "\n\n" "\n" "\n" 0 $ showMap "" showTableHeader show table



showTableHeader :: String -> String
showTableHeader header = if header /= "" then "[" ++ header ++ "]" ++ "\n"
                    else ""


showStr :: String -> String
showStr str = (if (length (lines str) > 1) && not (any (`elem` str) forbiddenChar)
                    then "| \n"
                    else if length (lines str) > 1 then "\n"
                    else "") ++

                   (if not $ any (`elem` str) forbiddenChar  then str
                   else if '"' `elem` str  then "'"  ++ indented str 3 ++ "'"
                   else                         "\"" ++ indented str 3) ++ "\""  where

  indented str n = head (lines str) ++
                    mconcat ((("\n" ++ replicate n ' ') ++) <$> tail (lines str))

  forbiddenChar = ['#', '&', '*', ',', '?', '-', ':', '[', ']', '{', '}'] ++
                  ['>', '|', ':', '!']
