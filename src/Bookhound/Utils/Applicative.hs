module Bookhound.Utils.Applicative where


extract :: Applicative m => m a1 -> m a2 -> m b -> m b
extract start end inner = start *> inner <* end
