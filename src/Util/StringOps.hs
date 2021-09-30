{-# LANGUAGE  FlexibleInstances, UndecidableInstances, IncoherentInstances  #-}

module Util.StringOps where


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
