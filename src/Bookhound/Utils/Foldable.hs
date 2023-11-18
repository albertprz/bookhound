module Bookhound.Utils.Foldable where

import Bookhound.Utils.String (indent)
import Control.Monad          (join)
import Data.Foldable          as Foldable (Foldable (toList), find)
import Data.List              (intercalate)
import Data.Maybe             (isJust)


stringify :: Foldable m => String -> String -> String -> Int -> m String -> String
stringify sep start end n xs = start <> indent n str <> end
  where
    str = intercalate sep list
    list = toList xs

findJust :: Foldable t => t (Maybe a) -> Maybe a
findJust ms = join $ Foldable.find isJust ms
