{-# LANGUAGE OverloadedStrings #-}

module Day8Spec (spec) where

import Data.Text (Text)
import Data.Text qualified as T (unlines)
import Day8 (part1, part2)
import Test.Hspec (Spec, describe, it, shouldBe)

example :: Text
example =
  T.unlines
    [ "............",
      "........0...",
      ".....0......",
      ".......0....",
      "....0.......",
      "......A.....",
      "............",
      "............",
      "........A...",
      ".........A..",
      "............",
      "............"
    ]

spec :: Spec
spec = do
  describe "part 1" $ do
    it "examples" $ do
      part1 example `shouldBe` 14
  describe "part 2" $ do
    it "examples" $ do
      part2 example `shouldBe` 34
