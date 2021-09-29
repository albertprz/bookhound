module Spec where

import Parsers (Parser)
import ParserCombinators ((|*), oneOf)

myParser = do spaces
              x <- (|*) $ oneOf ['a' .. 'z']
              oneOf [67, 89, 75]
              pure x

result = parse myParser "56   75"
