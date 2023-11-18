module Bookhound.Utils.List where

hasNone ::  [a] -> Bool
hasNone (_ : _) = False
hasNone _       = True

hasSome :: [a] -> Bool
hasSome (_ : _) = True
hasSome _       = False

hasMultiple :: [a] -> Bool
hasMultiple (_ : _ : _) = True
hasMultiple _           = False

headSafe :: [a] -> Maybe a
headSafe (x: _) = Just x
headSafe _      = Nothing
