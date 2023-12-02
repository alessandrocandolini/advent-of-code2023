{-# LANGUAGE QuasiQuotes #-}

module Day2Spec where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Day2
import NeatInterpolation
import Parser
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

exampleInput :: T.Text
exampleInput =
  [trimming|
        Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
        Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
        Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
        Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
        Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
     |]

exampleGames :: [Game]
exampleGames =
  [ Game
      1
      [ Sample [(Blue, 3), (Red, 4)]
      , Sample [(Red, 1), (Green, 2), (Blue, 6)]
      , Sample [(Green, 2)]
      ]
  , Game
      2
      [ Sample [(Blue, 1), (Green, 2)]
      , Sample [(Green, 3), (Blue, 4), (Red, 1)]
      , Sample [(Green, 1), (Blue, 1)]
      ]
  , Game
      3
      [ Sample [(Green, 8), (Blue, 6), (Red, 20)]
      , Sample [(Blue, 5), (Red, 4), (Green, 13)]
      , Sample [(Green, 5), (Red, 1)]
      ]
  , Game
      4
      [ Sample [(Green, 1), (Red, 3), (Blue, 6)]
      , Sample [(Green, 3), (Red, 6)]
      , Sample [(Green, 3), (Blue, 15), (Red, 14)]
      ]
  , Game
      5
      [ Sample [(Red, 6), (Blue, 1), (Green, 3)]
      , Sample [(Blue, 2), (Red, 1), (Green, 2)]
      ]
  ]

spec :: Spec
spec = describe "Day 2" $ do
  it "parse sample"
    $ parseAll sampleP " 3 blue, 4 red"
    `shouldBe` Just (Sample [(Blue, 3), (Red, 4)])

  it "parse gameId"
    $ parseAll gameIdP "Game 11:"
    `shouldBe` Just 11

  it "parseGames"
    $ parseGames exampleInput
    `shouldBe` exampleGames

  it "part1 example"
    $ (part1 . parseGames) exampleInput
    `shouldBe` 8

  it "solve the puzzle" $ do
    input <- T.readFile "resources/input2"
    logic input `shouldBe` Answer 2486 87984
