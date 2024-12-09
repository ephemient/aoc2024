{-# LANGUAGE OverloadedStrings #-}

module Day9Spec (spec) where

import Data.Text (Text)
import Data.Text qualified as T (unlines)
import Day9 (part1, part2)
import Test.Hspec (Spec, describe, it, shouldBe)

example :: Text
example =
  T.unlines
    [ "2333133121414131402"
    ]

spec :: Spec
spec = do
  describe "part 1" $ do
    it "examples" $ do
      part1 example `shouldBe` 1928
  describe "part 2" $ do
    it "examples" $ do
      part2 example `shouldBe` 2858
