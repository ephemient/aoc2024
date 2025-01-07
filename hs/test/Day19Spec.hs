{-# LANGUAGE OverloadedStrings #-}

module Day19Spec (spec) where

import Data.Text (Text)
import Data.Text qualified as T (unlines)
import Day19 (solve)
import Test.Hspec (Spec, describe, it, shouldBe)

example :: Text
example =
  T.unlines
    [ "r, wr, b, g, bwu, rb, gb, br",
      "",
      "brwrr",
      "bggr",
      "gbbr",
      "rrbgbr",
      "ubwu",
      "bwurrg",
      "brgr",
      "bbrgwb"
    ]

spec :: Spec
spec = do
  describe "solve" $ do
    it "examples" $ do
      solve example `shouldBe` Just (6, 16)
