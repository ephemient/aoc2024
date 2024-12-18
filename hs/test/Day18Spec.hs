{-# LANGUAGE OverloadedStrings #-}

module Day18Spec (spec) where

import Data.Text (Text)
import Data.Text qualified as T (unlines)
import Day18 (part1', part2')
import Test.Hspec (Spec, describe, it, shouldBe)

example :: Text
example =
  T.unlines
    [ "5,4",
      "4,2",
      "4,5",
      "3,0",
      "2,1",
      "6,3",
      "2,4",
      "1,5",
      "0,6",
      "3,3",
      "2,6",
      "5,1",
      "1,2",
      "5,5",
      "2,5",
      "6,5",
      "1,4",
      "0,4",
      "6,4",
      "1,1",
      "6,1",
      "1,0",
      "0,5",
      "1,6",
      "2,0"
    ]

spec :: Spec
spec = do
  describe "part 1" $ do
    it "examples" $ do
      part1' 6 12 example `shouldBe` Right 22
  describe "part 2" $ do
    it "examples" $ do
      part2' 6 example `shouldBe` Right (6, 1)
