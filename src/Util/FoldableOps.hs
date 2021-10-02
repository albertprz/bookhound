module Util.FoldableOps where

import Util.StringOps ( indent )
import Data.Foldable as Foldable ( Foldable(toList) )
import Data.List ( intercalate )


hasNone :: Foldable m => m a -> Bool
hasNone = null

hasSome :: Foldable m => m a -> Bool
hasSome = not . hasNone

hasMany :: Foldable m => m a -> Bool
hasMany xs = all hasSome $ [id, tail] <*> [Foldable.toList xs]


stringify :: (Foldable m) => String -> String -> String -> m String -> String
stringify sep start end xs = start ++ "\n" ++ indent 2 str ++ "\n" ++ end where

  str = intercalate (sep ++ "\n") list
  list = toList xs
