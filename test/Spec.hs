{-# LANGUAGE PostfixOperators #-}

import Parsers (number)
import ParserCombinators ((<#>), (|+), (|?))


myParser = do number
              (space |+)
              w1 <- lower <#> 3
              (inSet ['_', '-'] |?)
              w2 <- lower <#> 3
              (space |+)
              number

              pure [w1, w2]


result = parse myParser "56   abc-def   75"
