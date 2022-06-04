module SyntaxTrees.Yaml (YamlExpression(..), CollectionType(..)) where

import Utils.DateTime ()
import Utils.Foldable (stringify)
import Utils.Map      (showMap)

import           Data.Char (toLower)
import           Data.Map  (Map)
import qualified Data.Map  as Map
import           Data.Time (Day, TimeOfDay, ZonedTime (..))



data YamlExpression
  = YamlInteger Integer
  | YamlFloat Double
  | YamlBool Bool
  | YamlString String
  | YamlDate Day
  | YamlTime TimeOfDay
  | YamlDateTime ZonedTime
  | YamlList CollectionType [YamlExpression]
  | YamlMap CollectionType (Map String YamlExpression)
  | YamlNull
  deriving (Eq, Ord)

data CollectionType
  = Standard
  | Inline
  deriving (Eq, Ord)


instance Show YamlExpression where
  show = \case
    YamlNull                   -> "null"
    YamlInteger n              -> show n
    YamlFloat n                -> show n
    YamlBool bool              -> toLower <$> show bool
    YamlDate date              -> show date
    YamlTime time              -> show time
    YamlDateTime dateTime      -> show dateTime
    YamlString str             -> showStr str
    YamlList Standard list     -> stringify "\n" "\n" "" 2 $ ("- " ++) . show <$> list
    YamlMap  Standard mapping  -> stringify "\n" "\n" "" 2 $ showMap ": " id show mapping

    YamlList Inline   list     -> stringify (", " ++ sep) ("[ " ++ sep) (" ]" ++ sep) n $ show <$> list where
      (sep, n) = if (length . mconcat) (show <$> list) >= 80 then ("\n", 2) else ("", 0)

    YamlMap  Inline   mapping  -> stringify (", " ++ sep) ("{ " ++ sep) (" }" ++ sep) n $
                                    showMap ": " id show mapping where
      (sep, n) = if (length . mconcat) (show <$> Map.toList mapping) >= 80 then ("\n", 2) else ("", 0)



showStr :: String -> String
showStr str = (if (length (lines str) > 1) && not (any (`elem` str) forbiddenChar)
                    then "| \n"
                    else if length (lines str) > 1 then "\n"
                    else "") ++

                   (if not $ any (`elem` str) forbiddenChar  then str
                   else if '"' `elem` str  then "'"  ++ indented 3 ++ "'"
                   else                         "\"" ++ indented 3) ++ "\""  where

  indented n = head (lines str) ++
               mconcat ((("\n" ++ replicate n ' ') ++) <$> tail (lines str))

  forbiddenChar = ['#', '&', '*', ',', '?', '-', ':', '[', ']', '{', '}'] ++
                  ['>', '|', ':', '!']
