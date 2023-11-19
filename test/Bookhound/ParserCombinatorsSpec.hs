module Bookhound.ParserCombinatorsSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Instances.Text ()

import           Bookhound.Parser
import           Bookhound.ParserCombinators
import           Bookhound.Utils.List        (headSafe)
import           Control.Applicative         (optional)
import qualified Data.Foldable               as Foldable
import           Data.Text                   (Text, pack, unpack)
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

  describe "optional" $

    prop "applies a parser 1 or 0 times" $
      \x -> parse (optional anyChar) x
           ===
           (headSafe <$> parseTimes anyChar [0, 1] x)

  describe "many" $

    prop "applies a parser any number of times" $
      \x -> parse (many anyChar) x
           ===
           parseTimes anyChar [0 .. Text.length x + 10] x


  describe "some" $

    prop "applies a parser at least once" $
      \x -> parse (some anyChar) x
           ===
           parseTimes anyChar [1 .. Text.length x + 10] x

  describe "multiple" $

    prop "applies a parser at least twice" $
      \x -> parse (multiple anyChar) x
           ===
           parseTimes anyChar [2 .. Text.length x + 10] x

  describe "between" $

    prop "applies a parser surrounded by 2 parsers" $
      \x (y :: Char) (z :: Char) ->
        parse (between (is y) (is z) anyChar) x
        ===
        parse (is y *> anyChar <* is z) x

  describe "maybeBetween" $

    prop "applies a parser surrounded by 2 optional parsers" $
      \x (y :: Char) (z :: Char) ->
        parse (maybeBetween (is y) (is z) anyChar) x
        ===
        parse ((is y |?) *> anyChar <* (is z |?)) x

  describe "surroundedBy" $

    prop "applies a parser surrounded by a parser" $
      \x (y :: Char) ->
        parse (surroundedBy (is y) anyChar) x
        ===
        parse (is y *> anyChar <* is y) x

  describe "maybeSurroundedBy" $

    prop "applies a parser surrounded by a optional parsers" $
      \x (y :: Char) ->
        parse (maybeSurroundedBy (is y) anyChar) x
        ===
        parse ((is y |?) *> anyChar <* (is y |?)) x

  describe "manySepBy" $

    prop "applies a parser separated by a parser any number of times" $
      \x (y :: Char) ->
        parse (manySepBy (is y) anyChar) x
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

    prop "concats results of 2 parsers that can be converted to Texts" $
      \x (y :: Char) (z :: Char) ->
        parse (is y ->>- is z) x
        ===
        parse (is $ pack [y, z]) x


parseTimes :: Parser a -> [Int] -> Text -> ParseResult [a]
parseTimes p ns = parse $ anyOf ((`times` p) <$> reverse ns)

