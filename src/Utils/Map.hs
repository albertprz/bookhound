module Utils.Map where

import Data.Map (Map, elems, keys)


showMap :: String -> (String -> String) -> (a -> String) -> Map String a -> [String]
showMap sep showKey showValue mapping = (\(k, v) -> showKey k ++ sep ++ showValue v) <$> tuples where
  tuples = zip (keys mapping) (elems mapping)
