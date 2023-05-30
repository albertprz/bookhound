module Bookhound.Utils.List where


headSafe :: [a] -> Maybe a
headSafe (x: _) = Just x
headSafe _      = Nothing
