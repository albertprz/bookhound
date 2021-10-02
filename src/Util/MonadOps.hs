module Util.MonadOps where

import qualified Data.Foldable as Foldable
import GHC.OldList (intercalate)

extract :: Monad m => m a1 -> m a2 -> m b -> m b
extract start end inner = start *> inner <* end
