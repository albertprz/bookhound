{-# LANGUAGE PostfixOperators #-}

module Parsers.Xml (xml) where

import Parser (Parser)
import ParserCombinators (IsMatch(..), (<|>), (>>>), (|*), (|+))
import Parsers.Number (double)
import Parsers.Collections (listOf, mapOf)
import Parsers.Char (space)
import Parsers.String (withinDoubleQuotes, withinAngleBrackets, spacing, maybeWithinSpacing)
import SyntaxTrees.Xml ( XmlExpression(..), literalExpression )

import qualified Data.Map as Map
import Data.Map(Map)


word :: Parser String
word = (noneOf ['/', '>', ' ', '='] |+)


field :: Parser (String, String)
field = do x <- word
           is '='
           y <- string
           pure (x, y) where

  string = withinDoubleQuotes (isNot '"' |*)


fullTag :: Parser (String, Map String String)
fullTag = do tag  <- word
             flds <- Map.fromList <$> ((spacing *> field) |*)
             pure (tag, flds)


branchExpr :: Parser XmlExpression
branchExpr = do (tag, flds) <- withinAngleBrackets fullTag
                exprs       <- (xml |+) <|> literal
                maybeWithinSpacing (is ("</" ++ tag ++ ">"))
                pure $ XmlExpression { tagName = tag, fields = flds, expressions = exprs }


literal :: Parser [XmlExpression]
literal = (: []) . literalExpression <$> maybeWithinSpacing (noneOf "<" |*)


leafExpr :: Parser XmlExpression
leafExpr = do (tag, flds) <- withinAngleBrackets (fullTag <* is '/')
              pure $ XmlExpression { tagName = tag, fields = flds, expressions = [] }


header :: Parser String
header = maybeWithinSpacing $ is "<?" *> (isNot '?' |*) <* is "?>"


comment :: Parser String
comment = maybeWithinSpacing $ is "<!--" *> (isNot '-' |*) <* is "-->"



xml :: Parser XmlExpression
xml = (header <|> comment |*) *>
      maybeWithinSpacing (branchExpr <|> leafExpr)
