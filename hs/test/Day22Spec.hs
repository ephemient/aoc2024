{-# LANGUAGE OverloadedStrings #-}

module Day22Spec (spec) where

import Data.Text (Text)
import Data.Text qualified as T (unlines)
import Day22 (part1, part2)
import Test.Hspec (Spec, describe, it, shouldBe)

example1, example2 :: Text
example1 =
  T.unlines
    [ "1",
      "10",
      "100",
      "2024"
    ]
example2 =
  T.unlines
    [ "1",
      "2",
      "3",
      "2024"
    ]

spec :: Spec
spec = do
  describe "part 1" $ do
    it "examples" $ do
      part1 example1 `shouldBe` Right 37327623
  describe "part 2" $ do
    it "examples" $ do
      part2 example2 `shouldBe` Right 23
