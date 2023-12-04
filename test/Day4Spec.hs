{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Day4Spec where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Day4
import NeatInterpolation (trimming)
import Parser (parseAll)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = describe "Day 4" $ do
  it "numberP parses a one space separated set of numbers"
    $ parseAll numbersP "1 5 4 11 9"
    `shouldBe` Just [1, 5, 4, 11, 9]

  it "numberP parses a comma separated set of numbers with multiple space"
    $ parseAll numbersP "1  5 4 11    9"
    `shouldBe` Just [1, 5, 4, 11, 9]

  it "parse single card (including double space)"
    $ parseCard "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
    `shouldBe` Just (Card 1 [41, 48, 83, 86, 17] [83, 86, 6, 31, 17, 9, 48, 53])

  it "parse single card (with no double space)"
    $ parseCard "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19"
    `shouldBe` Just (Card 2 [13, 32, 20, 16, 61] [61, 30, 68, 82, 17, 32, 24, 19])

  it "parse card where winning numbers start in single digit (ie, there is an extra space)"
    $ parseCard "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1"
    `shouldBe` Just (Card 3 [1, 21, 53, 59, 44] [69, 82, 63, 72, 16, 21, 14, 1])

  it "parse card where numbers start in single digit (ie, there is an extra space)"
    $ parseCard "Card 3:  1 21 53 59 44 |  9 82 63 72 16 21 14  1"
    `shouldBe` Just (Card 3 [1, 21, 53, 59, 44] [9, 82, 63, 72, 16, 21, 14, 1])

  it "parse full input"
    $ parseInput
      [trimming|
        Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
        Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
        Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
        Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
        Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
        Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
     |]
    `shouldBe` [ Card 1 [41, 48, 83, 86, 17] [83, 86, 6, 31, 17, 9, 48, 53]
               , Card 2 [13, 32, 20, 16, 61] [61, 30, 68, 82, 17, 32, 24, 19]
               , Card 3 [1, 21, 53, 59, 44] [69, 82, 63, 72, 16, 21, 14, 1]
               , Card 4 [41, 92, 73, 84, 69] [59, 84, 76, 51, 58, 5, 54, 83]
               , Card 5 [87, 83, 26, 28, 32] [88, 30, 70, 12, 93, 22, 82, 36]
               , Card 6 [31, 18, 13, 56, 72] [74, 77, 10, 23, 35, 67, 36, 11]
               ]
  it "answer full input"
     $ (answer1 . parseInput)
      [trimming|
        Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
        Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
        Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
        Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
        Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
        Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
     |] `shouldBe` 13

  it "can parse all lines" $ do
    input <- T.readFile "resources/input4"
    length (parseInput input) `shouldBe` 202

  it "solve the puzzle" $ do
    input <- T.readFile "resources/input4"
    logic input `shouldBe` Answer  23678
