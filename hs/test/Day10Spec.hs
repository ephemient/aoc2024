{-# LANGUAGE OverloadedStrings #-}

module Day10Spec (spec) where

import Data.Text (Text)
import Data.Text qualified as T (unlines)
import Day10 (part1, part2)
import Test.Hspec (Spec, describe, it, shouldBe)

example :: Text
example =
  T.unlines
    [ "89010123",
      "78121874",
      "87430965",
      "96549874",
      "45678903",
      "32019012",
      "01329801",
      "10456732"
    ]

spec :: Spec
spec = do
  describe "part 1" $ do
    it "examples" $ do
      part1 example `shouldBe` 36
  describe "part 2" $ do
    it "examples" $ do
      part2 example `shouldBe` 81
