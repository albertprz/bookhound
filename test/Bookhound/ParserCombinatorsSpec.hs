module Bookhound.ParserCombinatorsSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Instances.Text ()

import           Bookhound.Parser
import           Bookhound.ParserCombinators
import           Bookhound.Utils.List        (headSafe)
import qualified Data.Foldable               as Foldable
import           Data.Text                   (Text, unpack)
import qualified Data.Text                   as Text
import           Test.QuickCheck.Property    ((===))

spec :: Spec
spec = do

  describe "times" $

    prop "applies a parser n times sequentially" $
      \x n -> parse (times n anyChar) x
           ===
            if Text.length x >= n then
               Result (Text.drop n x) (unpack $ Text.take n x)
            else
              Error UnexpectedEof

  describe "maybeTimes" $

    prop "applies a parser 1 or 0 times" $
      \x -> parse (maybeTimes anyChar) x
           ===
           (headSafe <$> parseTimes anyChar [0, 1] x)

  describe "anyTimes" $

    prop "applies a parser any number of times" $
      \x -> parse (anyTimes anyChar) x
           ===
           parseTimes anyChar [0 .. Text.length x] x


  describe "someTimes" $

    prop "applies a parser at least once" $
      \x -> parse (someTimes anyChar) x
           ===
           replaceError "someTimes"
             (parseTimes anyChar [1 .. Text.length x] x)

  describe "multipleTimes" $

    prop "applies a parser at least twice" $
      \x -> parse (multipleTimes anyChar) x
           ===
           replaceError "multipleTimes"
             (parseTimes anyChar [2 .. Text.length x] x)

  describe "withinBoth" $

    prop "applies a parser surrounded by 2 parsers" $
      \x (y :: Char) (z :: Char) ->
        parse (withinBoth (is y) (is z) anyChar) x
        ===
        parse (is y *> anyChar <* is z) x

  describe "maybeWithinBoth" $

    prop "applies a parser surrounded by 2 optional parsers" $
      \x (y :: Char) (z :: Char) ->
        parse (maybeWithinBoth (is y) (is z) anyChar) x
        ===
        parse ((is y |?) *> anyChar <* (is z |?)) x

  describe "within" $

    prop "applies a parser surrounded by a parser" $
      \x (y :: Char) ->
        parse (within (is y) anyChar) x
        ===
        parse (is y *> anyChar <* is y) x

  describe "maybeWithin" $

    prop "applies a parser surrounded by a optional parsers" $
      \x (y :: Char) ->
        parse (maybeWithin (is y) anyChar) x
        ===
        parse ((is y |?) *> anyChar <* (is y |?)) x

  describe "anySepBy" $

    prop "applies a parser separated by a parser any number of times" $
      \x (y :: Char) ->
        parse (anySepBy (is y) anyChar) x
        ===
        parse ((<>) <$> (Foldable.toList <$> (anyChar |?))
                    <*> ((is y *> anyChar) |*)) x

  describe "someSepBy" $

    prop "applies a parser separated by a parser at least once" $
      \x (y :: Char) ->
        parse (someSepBy (is y) anyChar) x
        ===
        parse ((:) <$> anyChar <*> ((is y *> anyChar) |*)) x

  describe "multipleSepBy" $

    prop "applies a parser separated by a parser at least twice" $
      \x (y :: Char) ->
        parse (multipleSepBy (is y) anyChar) x
        ===
        parse ((:) <$> anyChar <*> ((is y *> anyChar) |+)) x

  describe "->>-" $

    prop "concats results of 2 parsers that can be converted to Strings" $
      \x (y :: Char) (z :: Char) ->
        parse (is y ->>- is z) x
        ===
        parse (is [y, z]) x


parseTimes :: Parser a -> [Int] -> Text -> ParseResult [a]
parseTimes p ns = parse $ anyOf ((`times` p) <$> reverse ns)


replaceError :: String -> ParseResult a -> ParseResult a
replaceError err (Error (NoMatch _)) = Error $ NoMatch err
replaceError _ result                = result
