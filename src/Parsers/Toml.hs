module Parsers.Toml (toml, nil, integer, float, bool, string,
                     array, inlineTable) where

import Parser              (Parser)
import ParserCombinators   (IsMatch (..), maybeWithin, within, (<#>), (<|>),
                            (>>>), (|*), (|+), (|?))
import Parsers.Char        (dash, digit, dot, doubleQuote, equal, hashTag,
                            letter, newLine, quote, spaceOrTab, underscore,
                            whiteSpace)
import Parsers.Collections (listOf, mapOf)
import Parsers.Number      (double, hexInt, int, octInt)
import Parsers.String      (blankLine, blankLines, spacesOrTabs, spacing,
                            withinDoubleQuotes, withinQuotes,
                            withinSquareBrackets)
import SyntaxTrees.Toml    (TableType (..), TomlExpression (..))

import qualified Parsers.DateTime as Dt

import qualified Data.Map   as Map
import           Data.Maybe (maybeToList)



toml :: Parser TomlExpression
toml = maybeWithin (((pure <$> whiteSpace) <|> comment) |+) topLevelTable



-- TODO: Add support for table arrays

nil :: Parser TomlExpression
nil = TomlNull <$ is "null"

integer :: Parser TomlExpression
integer = TomlInteger <$> (hexInt <|> octInt <|> int)

float :: Parser TomlExpression
float = TomlFloat <$> double

bool :: Parser TomlExpression
bool = TomlBool <$> (True  <$ is "true"  <|>
                     False <$ is "false")


dateTime :: Parser TomlExpression
dateTime = TomlDateTime <$> Dt.dateTime

date :: Parser TomlExpression
date = TomlDate <$> Dt.date

time :: Parser TomlExpression
time = TomlTime <$> Dt.time


string :: Parser TomlExpression
string = TomlString <$> text where

  text = within (doubleQuote <#> 3) (multiline (inverse doubleQuote |*)) <|>
         within  (quote <#> 3)      (multiline (inverse quote |*))       <|>
         withinDoubleQuotes         (inverse doubleQuote |*)             <|>
         withinQuotes               (inverse quote       |*)

  multiline parser = mconcat <$> (((blankLine |?) *> line parser) |*)

  line parser = is "\\" *> (whiteSpace |*) *> parser


array :: Parser TomlExpression
array = TomlArray <$> listOf (maybeWithin spacing tomlExpr)


key :: Parser String
key = keyParser >>> ((dot >>> keyParser) |*) where

  keyParser = maybeWithin spacesOrTabs $ freeText      <|>
              withinDoubleQuotes (inverse doubleQuote |*) <|>
              withinQuotes (inverse quote |*)

  freeText = ((letter <|> digit <|> underscore <|> dash) |+)


inlineTable :: Parser TomlExpression
inlineTable = TomlTable Inline <$> mapOf equal key tomlExpr


topLevelTable :: Parser TomlExpression
topLevelTable = TomlTable TopLevel . Map.fromList <$> maybeWithin spacing tables where

  tables = do xs <- keyValueSeqParser
              ys <- (tableParser |*)
              pure (ys ++ [("", TomlTable Standard . Map.fromList $ xs)])

  tableParser = do k <- withinSquareBrackets key
                   v <- maybeWithin spacing standardTable
                   pure (k, v)

  standardTable = TomlTable Standard . Map.fromList <$> keyValueSeqParser


  keyValueSeqParser = do xs <- ((keyValueParser <* (blankLine *> (blankLines |?))) |*)
                         x  <- (keyValueParser |?)
                         pure  (xs ++ maybeToList x)

  keyValueParser = do k <- key
                      maybeWithin spacesOrTabs equal
                      v <- tomlExpr
                      pure (k, v)



element :: Parser TomlExpression
element = dateTime <|> date <|> time <|> float <|>
          integer  <|> bool <|> nil  <|> string


container :: Parser TomlExpression
container = array <|> inlineTable


comment :: Parser String
comment = hashTag *> (inverse newLine |+) <* newLine


tomlExpr :: Parser TomlExpression
tomlExpr = maybeWithin (((pure <$> spaceOrTab) <|> comment) |+) tomlValue where

  tomlValue = element <|> container
