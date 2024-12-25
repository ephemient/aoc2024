{-# LANGUAGE OverloadedStrings #-}

module Day25Spec (spec) where

import Data.Text (Text)
import Data.Text qualified as T (unlines)
import Day25 (part1)
import Test.Hspec (Spec, describe, it, shouldBe)

example :: Text
example =
  T.unlines
    [ -- :r!pbpaste | sed 's/.*/  , "&"/;1s/,/ /'
      "#####",
      ".####",
      ".####",
      ".####",
      ".#.#.",
      ".#...",
      ".....",
      "",
      "#####",
      "##.##",
      ".#.##",
      "...##",
      "...#.",
      "...#.",
      ".....",
      "",
      ".....",
      "#....",
      "#....",
      "#...#",
      "#.#.#",
      "#.###",
      "#####",
      "",
      ".....",
      ".....",
      "#.#..",
      "###..",
      "###.#",
      "###.#",
      "#####",
      "",
      ".....",
      ".....",
      ".....",
      "#....",
      "#.#..",
      "#.#.#",
      "#####"
    ]

spec :: Spec
spec = do
  describe "part 1" $ do
    it "examples" $ do
      part1 example `shouldBe` 3
