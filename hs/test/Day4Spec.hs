{-# LANGUAGE OverloadedStrings #-}

module Day4Spec (spec) where

import Data.Text (Text)
import Data.Text qualified as T (unlines)
import Day4 (part1, part2)
import Test.Hspec (Spec, describe, it, shouldBe)

example :: Text
example =
  T.unlines
    [ "MMMSXXMASM",
      "MSAMXMSMSA",
      "AMXSXMAAMM",
      "MSAMASMSMX",
      "XMASAMXAMM",
      "XXAMMXXAMA",
      "SMSMSASXSS",
      "SAXAMASAAA",
      "MAMMMXMMMM",
      "MXMXAXMASX"
    ]

spec :: Spec
spec = do
  describe "part 1" $ do
    it "examples" $ do
      part1 example `shouldBe` 18
  describe "part 2" $ do
    it "examples" $ do
      part2 example `shouldBe` 9
