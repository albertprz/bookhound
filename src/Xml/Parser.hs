{-# LANGUAGE PostfixOperators #-}

module Xml.Parser where

import ParserCombinators (Parser(parse), IsMatch(..), (<|>), (>>>), (|*), (|+))
import Parsers.Number (double)
import Parsers.Collections (listOf, mapOf)
import Parsers.Char (char, alphaNum)
import Parsers.String (withinDoubleQuotes, withinAngleBrackets, spacing, maybeWithinSpacing)
import Xml.Ast ( XmlExpression(..) )

import qualified Data.Map as Map
import Data.Map(Map)


word :: Parser String
word = (noneOf "/>" |+)


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
                exprs       <- maybeWithinSpacing (xml |*)
                maybeWithinSpacing (is ("</" ++ tag ++ ">"))
                pure $ XmlExpression tag flds exprs


leafExpr :: Parser XmlExpression
leafExpr = do (tag, flds) <- withinAngleBrackets (fullTag <* is '/')
              pure $ XmlExpression tag flds []


xml :: Parser XmlExpression
xml = branchExpr <|> leafExpr
