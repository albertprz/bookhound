module Utils.ArbitraryInstances where

import Test.QuickCheck
import Data.Text (pack, unpack, Text)

instance Arbitrary Text where
  arbitrary = pack <$> arbitrary
  shrink xs = pack <$> shrink (unpack xs)
