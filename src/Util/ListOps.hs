module Util.ListOps where

hasNone :: [a] -> Bool
hasNone = null

hasSome :: [a] -> Bool
hasSome = not . hasNone

hasMany :: [a] -> Bool
hasMany xs = all hasSome [xs, tail xs]
