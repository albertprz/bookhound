{-# LANGUAGE  FlexibleInstances, UndecidableInstances, IncoherentInstances  #-}

module Util.StringOps where
import Data.List (intercalate)


class ToString a where
  toString :: a -> String

instance ToString String where
  toString = id

instance ToString Char where
  toString = (: [])

instance (Num a, Show a) => ToString a where
  toString = show

instance (ToString a, Foldable m) => ToString (m a) where
  toString = concatMap toString



indent :: Int -> String -> String
indent n str = intercalate "\n" $ indentLine n <$> lines str where
  indentLine n = (concat (replicate n " ") ++)
