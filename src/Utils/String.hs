module Utils.String where
import Data.List (intercalate)


class ToString a where
  toString :: a -> String

instance ToString Char where
  toString = pure

instance ToString Int where
  toString = show

instance ToString Integer where
  toString = show

instance ToString Float where
  toString = show

instance ToString Double where
  toString = show

instance (ToString a, Foldable m) => ToString (m a) where
  toString = concatMap toString



indent :: Int -> String -> String
indent n str = intercalate "\n" $ indentLine <$> lines str where
  indentLine = (concat (replicate n " ") ++)
