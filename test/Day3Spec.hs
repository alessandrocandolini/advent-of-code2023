{-# LANGUAGE QuasiQuotes #-}

module Day3Spec where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Day3
import NeatInterpolation (trimming)
import Parser (parseAll)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = describe "Day 3" $ do
  it "parse grid"
    $ parseAll
      gridElementsP
      ( T.unpack
          [trimming|
        467..114..
        ...*......
        ..35..633.
        ......#...
        617*......
        .....+.58.
        ..592.....
        ......755.
        ...$.*....
        .664.598..
        |]
      )
    `shouldBe` Just
      [ [Digit 4, Digit 6, Digit 7, Blank, Blank, Digit 1, Digit 1, Digit 4, Blank, Blank]
      , [Blank, Blank, Blank, Symbol '*', Blank, Blank, Blank, Blank, Blank, Blank]
      , [Blank, Blank, Digit 3, Digit 5, Blank, Blank, Digit 6, Digit 3, Digit 3, Blank]
      , [Blank, Blank, Blank, Blank, Blank, Blank, Symbol '#', Blank, Blank, Blank]
      , [Digit 6, Digit 1, Digit 7, Symbol '*', Blank, Blank, Blank, Blank, Blank, Blank]
      , [Blank, Blank, Blank, Blank, Blank, Symbol '+', Blank, Digit 5, Digit 8, Blank]
      , [Blank, Blank, Digit 5, Digit 9, Digit 2, Blank, Blank, Blank, Blank, Blank]
      , [Blank, Blank, Blank, Blank, Blank, Blank, Digit 7, Digit 5, Digit 5, Blank]
      , [Blank, Blank, Blank, Symbol '$', Blank, Symbol '*', Blank, Blank, Blank, Blank]
      , [Blank, Digit 6, Digit 6, Digit 4, Blank, Digit 5, Digit 9, Digit 8, Blank, Blank]
      ]

  prop ""
    $ \l -> reverse (reverse l) == (l :: [Int])

  xit "solve the puzzle" $ do
    input <- T.readFile "resources/input3"
    logic input `shouldBe` Answer
