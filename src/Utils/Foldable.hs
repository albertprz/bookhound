module Utils.Foldable where

import Data.Foldable as Foldable (Foldable (toList))
import Data.List     (intercalate)
import Utils.String  (indent)


hasNone :: Foldable m => m a -> Bool
hasNone = null

hasSome :: Foldable m => m a -> Bool
hasSome = not . hasNone

hasMany :: Foldable m => m a -> Bool
hasMany xs = all hasSome $ [id, tail] <*> [Foldable.toList xs]


stringify :: (Foldable m) => String -> String -> String -> Int -> m String -> String
stringify sep start end n xs = start ++ indent n str ++ end where

  str = intercalate sep list
  list = toList xs
