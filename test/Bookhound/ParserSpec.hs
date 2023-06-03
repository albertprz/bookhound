module Bookhound.ParserSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Instances.Text ()

import Bookhound.Parser
import Data.Char
import Data.Text                (pack, unpack)
import Test.QuickCheck.Property ((===))



spec :: Spec
spec = do

  describe "Functor laws" $ do

    prop "Identity" $
      \x -> parse (id <$> char) x
           ===
           parse char x

    prop "Composition" $
      \x -> parse ((isLower . toUpper) <$> char) x
           ===
           parse ((isLower <$>) . (toUpper <$>) $ char) x


  describe "Applicative laws" $ do

    prop "Identity" $
      \x -> parse (pure id <*> char) x
           ===
           parse char x

    prop "Homomorphism" $
      \x (y :: [Int]) -> parse (pure sum <*> pure y) x
           ===
           parse (pure (sum y)) x

    prop "Interchange" $
      \x (y :: [Int])  -> parse (pure length <*> pure y) x
           ===
           parse (pure ($ y) <*> pure length) x


  describe "Monad laws" $ do

    prop "Left identity" $
      \x -> parse (pure 'a' >>= isMatch (==) char) x
           ===
           parse (isMatch (==) char 'a') x

    prop "Right identity" $
      \x -> parse (char >>= pure) x
           ===
           parse char x

    prop "Associativity" $
      \x -> parse
           (char >>= (\y -> isMatch (<) char y >>= isMatch (>) char)) x
           ===
           parse ((char >>= isMatch (<) char) >>= isMatch (>) char) x

  describe "char" $

    prop "parses a single char" $
      \x -> parse char x
           ===
           case unpack x of
             (ch : rest) -> Result (pack rest) ch
             []          -> Error UnexpectedEof

  describe "isMatch" $ do

    prop "works for ==" $
      \x y -> parse (isMatch (==) char y) x
             ===
             case unpack x of
               (ch : rest)
                 | ch == y    -> Result (pack rest) ch
                 | otherwise -> Error $ UnexpectedChar ch
               []            -> Error UnexpectedEof

    prop "works for /=" $
      \x y -> parse (isMatch (/=) char y) x
             ===
             case unpack x of
               (ch : rest)
                 | ch /= y    -> Result (pack rest) ch
                 | otherwise -> Error $ UnexpectedChar ch
               []            -> Error UnexpectedEof

  describe "check" $

    prop "performs a check on the parse result" $
      \x -> parse (check "digit" isDigit char) x
           ===
           case unpack x of
             (ch : rest)
               | isDigit ch -> Result (pack rest) ch
               | otherwise  -> Error $ NoMatch "digit"
             []             -> Error UnexpectedEof

  describe "except" $

    context "when the second parser fails" $
      prop "the first parser then runs" $
        \x -> parse (except char (char *> char)) x
             ===
             case unpack x of
               [ch]    -> Result (pack "") ch
               (_ : _) -> Error $ NoMatch "except"
               []      -> Error UnexpectedEof

  describe "anyOf" $

    prop "returns first parser success or last parser error if all fail" $
      \x -> parse (anyOf [errorParser $ ErrorAt "firstError",
                         "firstSuccess"  <$ char,
                         "secondSuccess" <$ char,
                         errorParser $ ErrorAt "secondError",
                         errorParser $ ErrorAt "lastError"]) x
           ===
           case unpack x of
             (_ : rest) -> Result (pack rest) "firstSuccess"
             []         -> Error $ ErrorAt "lastError"

  describe "allOf" $ do

    context "when any parser fails" $
      prop "returns first parser error" $
        \x -> parse (allOf ["firstSuccess"  <$ char,
                           "secondSuccess" <$ char,
                           errorParser $ ErrorAt "firstError"]) x
             ===
             case unpack x of
               (_ : _) -> Error $ ErrorAt "firstError"
               []      -> Error UnexpectedEof

    context "when all parsers succeed" $
      prop "returns last parser success" $
        \x -> parse (allOf ["firstSuccess"  <$ char,
                           "secondSuccess" <$ char,
                           "thirdSuccess"  <$ char]) x
             ===
             case unpack x of
               (_ : rest) -> Result (pack rest) "thirdSuccess"
               []         -> Error UnexpectedEof

  describe "exactly" $

    prop "returns an error when some chars remain after parsing" $
      \x -> parse (exactly char) x
           ===
           case unpack x of
             [ch]       -> Result (pack []) ch
             (_ : rest) -> Error $ ExpectedEof (pack rest)
             []         -> Error UnexpectedEof

  describe "runParser" $

    prop "parses exactly and wraps the result into an either" $
      \x -> runParser char x
           ===
           toEither (parse (exactly char) x)


  describe "withError" $

    prop "includes error labels in order when running the parser" $
      \x -> runParser ((,) <$> withErrorN 2 "firstErr" char
                          <*> withErrorN 1 "secondErr" char) x
           ===
           case unpack x of
             [ch1, ch2] -> Right (ch1, ch2)
             (_ : _ : rest)   -> Left [ErrorAt "firstErr",
                                      ErrorAt "secondErr",
                                      ExpectedEof (pack rest)]
             _                -> Left [ErrorAt "firstErr",
                                      ErrorAt "secondErr",
                                      UnexpectedEof]

  describe "withTransform" $

    prop "transforms current parser with provided fn" $
      \x -> parse (withTransform (\p -> char *> p <* char) char) x
           ===
           parse (char *> char <* char) x




toEither :: ParseResult a -> Either [ParseError] a
toEither = \case
  Result _ a -> Right a
  Error pe   -> Left [pe]
