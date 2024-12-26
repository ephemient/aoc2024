{-# LANGUAGE OverloadedStrings #-}

module Day17Spec (spec) where

import Data.Text (Text)
import Data.Text qualified as T (unlines)
import Day17 (part1, part2)
import Test.Hspec (Spec, describe, it, shouldBe)

example1, example2 :: Text
example1 =
  T.unlines
    [ "Register A: 729",
      "Register B: 0",
      "Register C: 0",
      "",
      "Program: 0,1,5,4,3,0"
    ]
example2 =
  T.unlines
    [ "Register A: 2024",
      "Register B: 0",
      "Register C: 0",
      "",
      "Program: 0,3,5,4,3,0"
    ]

spec :: Spec
spec = do
  describe "part 1" $ do
    it "examples" $ do
      part1 example1 `shouldBe` Right [4, 6, 3, 5, 6, 3, 5, 2, 1, 0]
  describe "part 2" $ do
    it "examples" $ do
      part2 example2 `shouldBe` Right 117440
