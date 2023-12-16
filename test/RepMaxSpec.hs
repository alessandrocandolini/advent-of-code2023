{-# LANGUAGE OverloadedLists #-}
module RepMaxSpec where

import RepMax
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.List.NonEmpty (NonEmpty)

spec :: Spec
spec = describe "Day N" $ do

  it "repMax"
    $ repMax ([1,5,2,3] :: [Int]) `shouldBe` [5,5,5,5]

  it "repMax1"
    $ repMax1 ([1,5,2,3] :: [Int]) `shouldBe` [5,5,5,5]

  it "repMax2"
    $ repMax2 ([1,5,2,3] :: [Int]) `shouldBe` [5,5,5,5]

  it "repMax2 on empty"
    $ repMax2 ([] :: [Int]) `shouldBe` []

  it "mean1"
    $ mean1 [1,2,3] `shouldBe` 2

  it "mean1 example2"
    $ mean1 [11,27,3, 7, 6] `shouldBe` 10.8

  it "meanDeviation"
    $ meanDeviation (-) [1,2,3]
    `shouldBe` [-1, 0, 1]

  prop "repMax and repMax1 produce same results"
    $ \l -> repMax l  == repMax1 (l :: [Int])

  prop "repMax2 and repMax1 produce same results"
    $ \l -> repMax l  == repMax2 (l :: [Int])
