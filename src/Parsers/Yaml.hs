{-# LANGUAGE PostfixOperators #-}

module Parsers.Yaml (yaml, nil, integer, float, bool, string, list, mapping) where


import SyntaxTrees.Yaml (YamlExpression(..), CollectionType(..))
import Parser(Parser(..), ParseError(..), errorParser, check, andThen, exactly)
import ParserCombinators (IsMatch(..), (<|>), (<#>), (>>>), (|?), (|*), (|+), (|++), maybeWithin)
import Parsers.Number (double, hexInt, int, octInt)
import Parsers.String (spaces, spacesOrTabs, withinDoubleQuotes, withinQuotes,
                        blankLine, blankLines, tabs)
import Parsers.Char (colon, dash, space, whiteSpace, newLine, question, dot, hashTag,
                     quote, doubleQuote, char)
import Parsers.Collections (mapOf, listOf)
import qualified Parsers.DateTime as Dt

import qualified Data.Map as Map
import Data.Map (Map)
import Data.List (nub)


-- TODO: Add support for table arrays

nil :: Parser YamlExpression
nil = YamlNull <$ oneOf ["null", "Null", "NULL"]

integer :: Parser YamlExpression
integer = YamlInteger <$> (hexInt <|> octInt <|> int)

float :: Parser YamlExpression
float = YamlFloat <$> double

bool :: Parser YamlExpression
bool = YamlBool <$> (True  <$ oneOf ["true", "True", "TRUE"])    <|>
                    (False <$ oneOf ["false", "False", "FALSE"])


dateTime :: Parser YamlExpression
dateTime = YamlDateTime <$> Dt.dateTime

date :: Parser YamlExpression
date = YamlDate <$> Dt.date

time :: Parser YamlExpression
time = YamlTime <$> Dt.time


string :: Int -> Parser YamlExpression
string indent = YamlString <$> text indent


sequential :: Parser a -> Int -> Parser [YamlExpression]
sequential sep indent = listParser where

  listParser = indentationCheck elemParser indent

  elemParser = do n <- length <$> (space |*)
                  sep *> whiteSpace
                  elem <- yamlWithIndent n
                  pure (n, elem)


list :: Int -> Parser YamlExpression
list indent = (YamlList Inline <$> jsonList) <|>
              (YamlList Standard <$> yamlList)  where

  yamlList = sequential dash indent
  jsonList = maybeWithin spacesOrTabs $ listOf $ yamlWithIndent (-1)


set :: Int -> Parser YamlExpression
set indent = YamlList Standard . nub <$> sequential question indent


mapping :: Int -> Parser YamlExpression
mapping indent = (YamlMap Inline <$> jsonMap) <|>
                 (YamlMap Standard <$> yamlMap)  where

  yamlMap = Map.fromList <$> mapParser
  jsonMap = maybeWithin spacesOrTabs $ mapOf colon (text 100) $ yamlWithIndent (-1)

  mapParser = indentationCheck keyValueParser indent

  keyValueParser = do n <- length <$> (space |*)
                      key <- show <$> element indent
                      (spacesOrTabs |?)
                      colon *> whiteSpace
                      value <- yamlWithIndent n
                      pure (n, (key, value))



element :: Int -> Parser YamlExpression
element indent = exactly (dateTime <|> date <|> time <|> float <|>
                          integer <|> bool <|> nil) <|>
                    string indent

container :: Int -> Parser YamlExpression
container indent = list indent <|> mapping indent <|> set indent



yamlWithIndent :: Int -> Parser YamlExpression
yamlWithIndent indent = maybeWithin ((blankLine <|> comment <|> directive <|>
                                      docStart <|> docEnd) |+)
                        yamlValue  where

  yamlValue = container indent <|> maybeWithin spacesOrTabs (element indent)

  comment = hashTag *> (inverse newLine |+) <* newLine

  directive = is "%" *> (inverse space *> (inverse newLine |+)) <* newLine

  docStart = dash <#> 3
  docEnd = dot <#> 3



yaml :: Parser YamlExpression
yaml =  normalize `andThen` yamlWithIndent (-1)



text :: Int -> Parser String
text indent = withinDoubleQuotes (quotedParser (inverse doubleQuote |*))                      <|>
              withinQuotes       (quotedParser (inverse quote       |*))                      <|>
              (is "|" *> blankLine *> plainTextParser literalLineParser)                      <|>
              (is ">" *> blankLine *> (spacesOrTabs |?) *> plainTextParser foldingLineParser) <|>
              plainTextParser foldingLineParser
  where

  quotedParser parser = mconcat <$> ((snd <$> foldingLineParser parser) |*)

  plainTextParser styleParser = allowedStart >>> allowedString >>>
                                (indentationCheck (styleParser allowedString) indent |*)

  foldingLineParser parser = do sep <- ("\n" <$ newLine <* blankLines) <|> (" " <$ newLine)
                                n   <- maybeWithin tabs $ length <$> (space |*)
                                str <- parser
                                pure (n, sep ++ str)

  literalLineParser parser = do sep <- pure <$> newLine
                                n   <- length <$> (space |*)
                                str <- parser
                                pure (n, sep ++ replicate (n - indent) ' ' ++ str)

  allowedStart = noneOf $ forbiddenChar ++ ['>', '|', ':', '!']

  allowedString = (noneOf forbiddenChar |*)

  forbiddenChar = ['\n', '#', '&', '*', ',', '?', '-', ':', '[', ']', '{', '}']



indentationCheck :: Parser (Int, a) -> Int -> Parser [a]
indentationCheck parser indent = ((snd <$> check "indentation"
                                  (\(n, _) -> n > indent) parser) |+)


normalize :: Parser String
normalize = (parserActions >>> normalize) <|> (char |*) where

  parserActions = spreadDashes     <|>
                  spreadDashKey    <|>
                  spreadKeyDash    <|>
                  next

  next = pure <$> char

  spreadDashes = (++ "- ") . genDashes <$> dashesParser

  genDashes (offset, n) = concatMap (\x -> "- " ++ replicate (offset + 2 * x) ' ')
                                    [1 .. n - 1]

  dashesParser = do offset <- length <$> (spaces |?)
                    n <- length <$> ((dash <* spacesOrTabs) |++)
                    pure (offset, n)


  spreadDashKey = (\(offset, key) -> replicate offset ' ' ++ "- " ++
                                     replicate (offset + 2) ' ' ++ key ++ ": ")
                  <$> dashKeyParser

  dashKeyParser = do offset <- length <$> (spaces |?)
                     dash <* spacesOrTabs
                     key <- text 100 <* maybeWithin spacesOrTabs colon
                     pure (offset, key)


  spreadKeyDash = (\(offset, key) -> replicate offset ' ' ++ key ++ ": " ++
                                     replicate (offset + 2) ' ' ++ "- ")
                  <$> keyDashParser

  keyDashParser = do offset <- length <$> (spaces |?)
                     key <- text 100 <* maybeWithin spacesOrTabs colon
                     dash <* spacesOrTabs
                     pure (offset, key)
