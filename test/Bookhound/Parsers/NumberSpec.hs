module Bookhound.Parsers.NumberSpec where

import Test.Hspec
import Test.Hspec.QuickCheck

import Test.QuickCheck                ((==>))
import Test.QuickCheck.Instances.Text ()

import Bookhound.Parser
import Bookhound.Parsers.Number
import Data.Text                (pack)
import Test.QuickCheck.Property ((===))



spec :: Spec
spec = do

  describe "posInt" $

    prop "parses a positive Int" $
      \(x :: Int) -> x > 0 ==>
        runParser posInt (pack $ show x)
        ===
        Right x

  describe "negInt" $

    prop "parses a negative Int" $
      \(x :: Int) -> x < 0 ==>
        runParser negInt (pack $ show x)
        ===
        Right x

  describe "int" $

    prop "parses an Int" $
      \(x :: Int) ->
        runParser int (pack $ show x)
        ===
        Right x

