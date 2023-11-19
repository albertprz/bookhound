module Bookhound.Parsers.Number (int, double, posInt, negInt, unsignedInt, hexInt, octInt) where

import Bookhound.Parser            (Parser, withErrorN)
import Bookhound.ParserCombinators (IsMatch (..), string, (->>-), (|?), (||+),
                                    (||?))
import Bookhound.Parsers.Char      (dash, digit, dot, plus)
import Control.Applicative
import Data.Text                   (Text, unpack)


hexInt :: Parser Int
hexInt = withErrorN (-1) "Hex Int" $ readInt
  <$> (string "0x" ->>- ((digit <|> oneOf ['A' .. 'F'] <|> oneOf ['a' .. 'f']) ||+))

octInt :: Parser Int
octInt = withErrorN (-1) "Oct Int" $ readInt
  <$> (string "0o" ->>- (oneOf ['0' .. '7'] ||+))

unsignedInt :: Parser Int
unsignedInt = withErrorN (-1) "Unsigned Int" $ readInt
  <$> (digit ||+)

posInt :: Parser Int
posInt = withErrorN (-1) "Positive Int" $ readInt
  <$> ((plus ||?) *> (digit ||+))

negInt :: Parser Int
negInt = withErrorN (-1) "Negative Int" $ readInt
  <$> dash ->>- (digit ||+)

int :: Parser Int
int = withErrorN (-1) "Int" $ negInt <|> posInt

double :: Parser Double
double = withErrorN (-1) "Double" $ readDouble
  <$> int ->>- (decimals ->>- (expn |?) <|> expn)
  where
  decimals = dot ->>- (digit ||+)
  expn      = oneOf ['e', 'E'] ->>- int

readInt :: Text -> Int
readInt = read . unpack

readDouble :: Text -> Double
readDouble = read . unpack
