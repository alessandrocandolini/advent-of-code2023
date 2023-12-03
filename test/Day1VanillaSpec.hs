{-# LANGUAGE QuasiQuotes #-}

module Day1VanillaSpec where

import qualified Data.Text as T
import Day1Vanilla
import NeatInterpolation
import Test.Hspec

spec :: Spec
spec = describe "Day 1" $ do
  it "logic"
    $ part1 (T.unpack
      [trimming|1abc2
      pqr3stu8vwx
      a1b2c3d4e5f
      treb7uchet|] )
    `shouldBe` [12, 38, 15, 77]

  it "parseAll example 1"
    $ parseAll "123"
    `shouldBe` ['1', '2', '3']

  it "parseAll example 2"
    $ parseAll "1a2b3"
    `shouldBe` ['1', '2', '3']

  it "parseAll example 3"
    $ parseAll "one2twone"
    `shouldBe` ['1', '2', '2', '1']

  it "parseAll example 4"
    $ parseAll "aoneb2ttwone"
    `shouldBe` ['1', '2', '2', '1']

  it "parseAll example 5"
    $ parseAll "aoneon2eighthree"
    `shouldBe` ['1', '2', '8', '3']

  it "parseAll example 6"
    $ parseAll "eightwothree"
    `shouldBe` ['8', '2', '3']

  it "parseAll example 6"
    $ parseAll "threeeightwo"
    `shouldBe` ['3', '8', '2']

  it "part2"
    $ part2 (T.unpack
      [trimming|
        two1nine
        eightwothree
        abcone2threexyz
        xtwone3four
        4nineeightseven2
        zoneight234
        7pqrstsixteen
      |])
    `shouldBe` [29, 83, 13, 24, 42, 14, 76]

  it "solve the puzzle" $ do
    input <- readFile "resources/input1"
    logic input `shouldBe` (55712, 55413)
