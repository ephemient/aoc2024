{-# LANGUAGE OverloadedStrings #-}

module Day1Spec (spec) where

import Data.Text (Text)
import Data.Text qualified as T (unlines)
import Day1 (part1, part2)
import Test.Hspec (Spec, describe, it, shouldBe)

example :: Text
example =
  T.unlines
    [ "3   4",
      "4   3",
      "2   5",
      "1   3",
      "3   9",
      "3   3"
    ]

spec :: Spec
spec = do
  describe "part 1" $ do
    it "examples" $ do
      part1 example `shouldBe` Right 11
  describe "part 2" $ do
    it "examples" $ do
      part2 example `shouldBe` Right 31
