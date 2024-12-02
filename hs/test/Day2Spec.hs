{-# LANGUAGE OverloadedStrings #-}

module Day2Spec (spec) where

import Data.Text (Text)
import Data.Text qualified as T (unlines)
import Day2 (part1, part2)
import Test.Hspec (Spec, describe, it, shouldBe)

example :: Text
example =
  T.unlines
    [ "7 6 4 2 1",
      "1 2 7 8 9",
      "9 7 6 2 1",
      "1 3 2 4 5",
      "8 6 4 4 1",
      "1 3 6 7 9"
    ]

spec :: Spec
spec = do
  describe "part 1" $ do
    it "examples" $ do
      part1 example `shouldBe` Right 2
  describe "part 2" $ do
    it "examples" $ do
      part2 example `shouldBe` Right 4
