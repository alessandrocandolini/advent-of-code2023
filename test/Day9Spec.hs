module Day9Spec where

import Day9
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import qualified Data.Text as T
import qualified Data.Text.IO as T

spec :: Spec
spec = describe "Day 5" $ do
  it "zipWithNext of a list with size at least 2"
    $ zipWithNext [1,2,3,4,5,6,7,8]
    `shouldBe` [(1,2), (2,3), (3,4), (4,5), (5,6), (6,7), (7,8)]

  it "zipWithNext of a list with size 1"
    $ zipWithNext [1]
    `shouldBe` []

  it "zipWithNext of a empty list"
    $ zipWithNext ([] :: [Int])
    `shouldBe` []

  it "diffWithNext of a list with size at least 2"
    $ diffWithNext [1, 3, 6, 10, 15, 21]
    `shouldBe` [2, 3, 4, 5, 6]

  it "zipWithNext of a list with size 1"
    $ zipWithNext [1]
    `shouldBe` []

  it "zipWithNext of a empty list"
    $ zipWithNext ([] :: [Int])
    `shouldBe` []

  prop ""
    $ \l -> reverse (reverse l) == (l :: [Int])

  xit "solve the puzzle" $ do
     input <- T.readFile "resources/input9"
     logic input `shouldBe` Answer
