{-# LANGUAGE PostfixOperators #-}

module Parsers.Yaml where

import Parser(Parser(..), ParseError(..), errorParser, check, andThen)
import ParserCombinators (IsMatch(..), (<|>), (<&>), (>>>), (<#>), (|*), (|?), (|+), (|++))
import Parsers.Number (double, int, hexInt, octInt)
import Parsers.String (withinDoubleQuotes, withinQuotes, within, maybeWithin, word,
                      spacesOrTabs, newLines, spaces, blankLines)
import Parsers.Char (dash, hashTag, doubleQuote, quote, colon,
                     space, newLine, whiteSpace, char)
import SyntaxTrees.Yaml (YamlExpression(..))

import qualified Data.Map as Map
import Data.Map (Map)


nil :: Parser YamlExpression
nil = YamlNull <$ is "null"

integer :: Parser YamlExpression
integer = YamlInteger <$> (int <|> hexInt <|> octInt)

float :: Parser YamlExpression
float = YamlFloat <$> double

bool :: Parser YamlExpression
bool = YamlBool <$> (True  <$ oneOf ["true", "True", "TRUE"])    <|>
                    (False <$ oneOf ["false", "False", "FALSE"])


string :: Parser YamlExpression
string = YamlString <$> text


list :: Int -> Parser YamlExpression
list indent = YamlList <$> listParser  where

  listParser = ((snd <$> check "indentation" (\(n, _) -> n > indent) elemParser) |+)

  elemParser = do n <- length <$> (space |*)
                  dash *> whiteSpace
                  elem <- yamlWithIndent n
                  pure (n, elem)


mapping :: Int -> Parser YamlExpression
mapping indent = YamlMap . Map.fromList <$> mapParser  where

  mapParser = ((snd <$> check "indentation" (\(n, _) -> n > indent) keyValueParser) |+)

  keyValueParser = do n <- length <$> (space |*)
                      key <- text
                      (spacesOrTabs |?)
                      colon *> whiteSpace
                      value <- yamlWithIndent n
                      pure (n, (key, value))


text :: Parser String
text = (noneOf ['\n', ':', '!', '-', '#', '&', '*'] >>> (noneOf ['\n', '#', '&', '*'] |*))  <|>
       withinDoubleQuotes (inverse doubleQuote |*)                                          <|>
       withinQuotes       (inverse quote       |*)


comment :: Parser String
comment = hashTag *> (inverse space *> (inverse newLine |+)) <* newLine


yamlWithIndent :: Int -> Parser YamlExpression
yamlWithIndent indent = maybeWithin blankLines yamlValue  where

  yamlValue = yamlContainer <|> maybeWithin spacesOrTabs yamlElem

  yamlContainer = list indent <|> mapping indent

  yamlElem = integer <|> float <|> bool <|> nil <|> string



yaml :: Parser YamlExpression
yaml =  normalizeYaml `andThen` yamlWithIndent (-1) where

  normalizeYaml = spreadDashes             <|>
                  spreadDashKey            <|>
                  spreadKeyDash            <|>
                  stripComment             <|>
                  (char >>> normalizeYaml) <|>
                  (char |*)

  dashesParser = do offset <- length <$> (spaces |?)
                    n <- length <$> ((dash <* spacesOrTabs) |++)
                    pure (offset, n)

  spreadDashes = ((++ "- ") . genDashes <$> dashesParser) >>> normalizeYaml

  genDashes (offset, n) = concatMap (\x -> "- " ++ replicate (offset + 2 * x) ' ')
                                    [1 .. n - 1]


  dashKeyParser = do offset <- length <$> (spaces |?)
                     dash <* spacesOrTabs
                     key <- text <* maybeWithin spacesOrTabs colon
                     pure (offset, key)

  spreadDashKey = ((\(offset, key) -> replicate offset ' ' ++ "- " ++
                                      replicate (offset + 2) ' ' ++ key ++ ": ")
                  <$> dashKeyParser) >>> normalizeYaml


  keyDashParser = do offset <- length <$> (spaces |?)
                     key <- text <* maybeWithin spacesOrTabs colon
                     dash <* spacesOrTabs
                     pure (offset, key)

  spreadKeyDash = ((\(offset, key) -> replicate offset ' ' ++ key ++ ": " ++
                                      replicate (offset + 2) ' ' ++ "- ")
                  <$> keyDashParser) >>> normalizeYaml

  stripComment = ("\n" <$ comment) >>> normalizeYaml
