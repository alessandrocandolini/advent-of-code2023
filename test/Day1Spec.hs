{-# LANGUAGE QuasiQuotes #-}

module Day1Spec where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Day1
import NeatInterpolation
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import qualified Text.Megaparsec as P

parsePart2 :: T.Text -> Maybe [Digit]
parsePart2 = parse parser2 . T.unpack

spec :: Spec
spec = describe "Day 1" $ do
  it "logic"
    $ part1
      [trimming|1abc2
      pqr3stu8vwx
      a1b2c3d4e5f
      treb7uchet|]
    `shouldBe` [12, 38, 15, 77]

  it "parsePart2 example 1"
    $ parsePart2 "123"
    `shouldBe` Just [One, Two, Three]

  it "parsePart2 example 2"
    $ parsePart2 "1a2b3"
    `shouldBe` Just [One, Two, Three]

  it "parsePart2 example 3"
    $ parsePart2 "one2twone"
    `shouldBe` Just [One, Two, Two, One]

  it "parsePart2 example 4"
    $ parsePart2 "aoneb2ttwone"
    `shouldBe` Just [One, Two, Two, One]

  it "parsePart2 example 5"
    $ parsePart2 "aoneon2eighthree"
    `shouldBe` Just [One, Two, Eight, Three]

  it "parsePart2 example 6"
    $ parsePart2 "eightwothree"
    `shouldBe` Just [Eight, Two, Three]

  it "parsePart2 example 6"
    $ parsePart2 "threeeightwo"
    `shouldBe` Just [Three, Eight, Two]

  it "part2"
    $ part2
      [trimming|
        two1nine
        eightwothree
        abcone2threexyz
        xtwone3four
        4nineeightseven2
        zoneight234
        7pqrstsixteen
      |]
    `shouldBe` [29, 83, 13, 24, 42, 14, 76]

  it "solve the puzzle" $ do
    input <- T.readFile "resources/input1"
    logic input `shouldBe` Answer 55712 55413
