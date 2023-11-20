module Bookhound.Utils.Text where

import qualified Data.Foldable as Foldable
import           Data.List     (intercalate)
import           Data.Text     (Text, pack)
import qualified Data.Text     as Text


class ToText a where
  toText :: a -> Text

instance ToText Char where
  toText = Text.singleton

instance ToText Int where
  toText = pack . show

instance ToText Text where
  toText = id

instance ToText Integer where
  toText = pack . show

instance ToText Float where
  toText = pack .show

instance ToText Double where
  toText = pack . show

instance {-# OVERLAPPING #-} ToText a => ToText [a] where
  toText = Text.concat . fmap toText

instance (ToText a, Foldable f, Functor f) => ToText (f a) where
  toText = Text.concat . Foldable.toList . fmap toText


indent :: Int -> String -> String
indent n str = intercalate "\n" $ indentLine <$> lines str
  where
    indentLine = (concat (replicate n " ") <>)
