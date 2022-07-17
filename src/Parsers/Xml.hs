module Parsers.Xml (xml, branchExpr, leafExpr, literal) where

import Parser            (Parser, withError)
import ParserCombinators (IsMatch (..), maybeWithin, (<|>), (|*), (|+))
import Parsers.Char      (doubleQuote)
import Parsers.String    (spacing, withinAngleBrackets, withinDoubleQuotes)
import SyntaxTrees.Xml   (XmlExpression (..), literalExpression)

import           Data.Map (Map)
import qualified Data.Map as Map


xml :: Parser XmlExpression
xml = maybeWithin  ((header <|> comment) |+)
    $ maybeWithin spacing $ branchExpr <|> leafExpr



field :: Parser (String, String)
field = withError "Xml Field" $
  (,) <$> text <* is '=' <*> quotedText
  where
    quotedText = withinDoubleQuotes (inverse doubleQuote |*)


fullTag :: Parser (String, Map String String)
fullTag = withError "Xml Tag" $
  do tag  <- text
     flds <- Map.fromList <$> ((spacing *> field) |*)
     pure (tag, flds)


branchExpr :: Parser XmlExpression
branchExpr = withError "Xml Branch Expression" $
  do (tag, flds) <- withinAngleBrackets fullTag
     exprs       <- (xml |+) <|> literal
     maybeWithin spacing (is ("</" <> tag <> ">"))
     pure $ XmlExpression { tagName = tag, fields = flds, expressions = exprs }


literal :: Parser [XmlExpression]
literal = withError "Xml Literal" $
  pure . literalExpression <$> maybeWithin spacing (isNot '<' |*)


leafExpr :: Parser XmlExpression
leafExpr = withError "Xml Leaf Expression" $
  do (tag, flds) <- withinAngleBrackets (fullTag <* is '/')
     pure $ XmlExpression { tagName = tag, fields = flds, expressions = [] }


header :: Parser String
header = maybeWithin spacing $
  is "<?" *> (isNot '?' |*) <* is "?>"


comment :: Parser String
comment = maybeWithin spacing $
  is "<!--" *> (isNot '-' |*) <* is "-->"



text :: Parser String
text = (noneOf ['/', '>', ' ', '='] |+)
