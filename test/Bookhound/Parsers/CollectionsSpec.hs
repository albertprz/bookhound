module Bookhound.Parsers.CollectionsSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Instances.Text ()

import           Bookhound.Parser
import           Bookhound.ParserCombinators   (IsMatch (is))
import           Bookhound.Parsers.Char        (alpha)
import           Bookhound.Parsers.Collections
import           Data.Char                     (isAlpha, isAscii)
import           Data.List                     (intercalate)
import qualified Data.Map                      as Map
import           Data.Text                     (pack)
import           Test.QuickCheck               ((==>))



spec :: Spec
spec = do

  describe "listOf" $

    prop "parses a list provided the element parser" $
      \(x :: [Char]) ->
        runParser (listOf alpha)
        (pack ("[" <>
               intercalate ", " (pure <$> filter isAlpha' x)
               <> "]"))
       `shouldBe`
        Right (filter isAlpha' x)

  describe "tupleOf" $

    prop "parses a tuple provided the element parser" $
      \(x :: [Char]) -> length (filter isAlpha' x) >= 2 ==>
        runParser (tupleOf alpha)
        (pack ("(" <>
               intercalate ", " (pure <$> filter isAlpha' x)
               <> ")"))
       `shouldBe`
        Right (filter isAlpha' x)

  describe "mapOf" $

    prop "parses a map provided the key and value parsers" $
      \(x :: [(Char, Char)]) ->
        runParser (mapOf (is "->") alpha alpha)
        (pack ("{" <>
               intercalate ", " (showMapEntry <$> filter areAlpha' x)
               <> "}"))
       `shouldBe`
        Right (Map.fromList $ filter areAlpha' x)



showMapEntry :: (Char, Char) -> String
showMapEntry (x, y) = pure x <> " -> " <> pure y

areAlpha' :: (Char, Char) -> Bool
areAlpha' (x, y) = isAlpha' x && isAlpha' y

isAlpha' :: Char -> Bool
isAlpha' x = isAlpha x && isAscii x
