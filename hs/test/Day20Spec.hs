{-# LANGUAGE OverloadedStrings #-}

module Day20Spec (spec) where

import Data.Text (Text)
import Data.Text qualified as T (unlines)
import Day20 (solve)
import Test.Hspec (Spec, describe, it, shouldBe)

example :: Text
example =
  T.unlines
    [ "###############",
      "#...#...#.....#",
      "#.#.#.#.#.###.#",
      "#S#...#.#.#...#",
      "#######.#.#.###",
      "#######.#.#...#",
      "#######.#.###.#",
      "###..E#...#...#",
      "###.#######.###",
      "#...###...#...#",
      "#.#####.#.###.#",
      "#.#...#.#.#...#",
      "#.#.#.#.#.#.###",
      "#...#...#...###",
      "###############"
    ]

spec :: Spec
spec = do
  describe "part 1" $ do
    it "examples" $ do
      solve 2 `flip` example <$> [2, 4, 6, 8, 10, 12, 20, 36, 38, 40, 64, 100]
        `shouldBe` scanr (+) 0 [14, 14, 2, 4, 2, 3, 1, 1, 1, 1, 1]
  describe "part 2" $ do
    it "examples" $ do
      solve 20 `flip` example <$> [50, 52, 54, 56, 58, 60, 62, 64, 66, 68, 70, 72, 74, 76, 100]
        `shouldBe` scanr (+) 0 [32, 31, 29, 39, 25, 23, 20, 19, 12, 14, 12, 22, 4, 3]
