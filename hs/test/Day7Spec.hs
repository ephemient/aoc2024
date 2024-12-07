{-# LANGUAGE OverloadedStrings #-}

module Day7Spec (spec) where

import Data.Text (Text)
import Data.Text qualified as T (unlines)
import Day7 (part1, part2)
import Test.Hspec (Spec, describe, it, shouldBe)

example :: Text
example =
  T.unlines
    [ "190: 10 19",
      "3267: 81 40 27",
      "83: 17 5",
      "156: 15 6",
      "7290: 6 8 6 15",
      "161011: 16 10 13",
      "192: 17 8 14",
      "21037: 9 7 18 13",
      "292: 11 6 16 20"
    ]

spec :: Spec
spec = do
  describe "part 1" $ do
    it "examples" $ do
      part1 example `shouldBe` Right 3749
  describe "part 2" $ do
    it "examples" $ do
      part2 example `shouldBe` Right 11387
