{-# LANGUAGE PostfixOperators #-}

module Normalizers.Yaml where


import Parser(Parser, check)
import ParserCombinators (IsMatch(..), (<|>), (>>>), (|?), (|*), (|+), (|++), maybeWithin)
import Parsers.String (spaces, spacesOrTabs, withinDoubleQuotes,
                       withinQuotes, blankLine, blankLines, tabs)
import Parsers.Char (char, colon, dash, doubleQuote, hashTag, newLine, quote, space)


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


comment :: Parser String
comment = hashTag *> (inverse space *> (inverse newLine |+)) <* newLine

directive :: Parser String
directive = is "%" *> (inverse space *> (inverse newLine |+)) <* newLine


indentationCheck :: Parser (Int, a) -> Int -> Parser [a]
indentationCheck parser indent = ((snd <$> check "indentation"
                                  (\(n, _) -> n > indent) parser) |+)

normalize :: Parser String
normalize = (parserActions >>> normalize) <|> (char |*) where

  parserActions = spreadDashes     <|>
                  spreadDashKey    <|>
                  spreadKeyDash    <|>
                  stripComment     <|>
                  stripDirective   <|>
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


  stripComment = "\n" <$ comment

  stripDirective = "\n" <$ directive
