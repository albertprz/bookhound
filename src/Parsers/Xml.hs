{-# LANGUAGE PostfixOperators #-}

module Parsers.Xml (xml, branchExpr, leafExpr, literal) where

import Parser (Parser)
import ParserCombinators (IsMatch(..), (<|>), (>>>), (|*), (|+))
import Parsers.Number (double)
import Parsers.Collections (listOf, mapOf)
import Parsers.Char (space, doubleQuote)
import Parsers.String (withinDoubleQuotes, withinAngleBrackets, spacing, maybeWithin)
import SyntaxTrees.Xml ( XmlExpression(..), literalExpression )

import qualified Data.Map as Map
import Data.Map(Map)



field :: Parser (String, String)
field = do x <- text
           is '='
           y <- quotedText
           pure (x, y) where

  quotedText = withinDoubleQuotes (inverse doubleQuote |*)


fullTag :: Parser (String, Map String String)
fullTag = do tag  <- text
             flds <- Map.fromList <$> ((spacing *> field) |*)
             pure (tag, flds)


branchExpr :: Parser XmlExpression
branchExpr = do (tag, flds) <- withinAngleBrackets fullTag
                exprs       <- (xml |+) <|> literal
                maybeWithin spacing (is ("</" ++ tag ++ ">"))
                pure $ XmlExpression { tagName = tag, fields = flds, expressions = exprs }


literal :: Parser [XmlExpression]
literal = pure . literalExpression <$> maybeWithin spacing (isNot '<' |*)


leafExpr :: Parser XmlExpression
leafExpr = do (tag, flds) <- withinAngleBrackets (fullTag <* is '/')
              pure $ XmlExpression { tagName = tag, fields = flds, expressions = [] }


header :: Parser String
header = maybeWithin spacing $ is "<?" *> (isNot '?' |*) <* is "?>"


comment :: Parser String
comment = maybeWithin spacing $ is "<!--" *> (isNot '-' |*) <* is "-->"



xml :: Parser XmlExpression
xml = maybeWithin  ((header <|> comment) |+) $
        maybeWithin spacing $ branchExpr <|> leafExpr


text :: Parser String
text = (noneOf ['/', '>', ' ', '='] |+)
