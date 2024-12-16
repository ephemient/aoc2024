{-# LANGUAGE OverloadedStrings #-}

module Day16Spec (spec) where

import Data.Text (Text)
import Data.Text qualified as T (unlines)
import Day16 (part1, part2)
import Test.Hspec (Spec, describe, it, shouldBe)

example1, example2 :: Text
example1 =
  T.unlines
    [ -- :r!pbpaste | sed 's/.*/  , "&"/;1s/,/ /'
      "###############",
      "#.......#....E#",
      "#.#.###.#.###.#",
      "#.....#.#...#.#",
      "#.###.#####.#.#",
      "#.#.#.......#.#",
      "#.#.#####.###.#",
      "#...........#.#",
      "###.#.#####.#.#",
      "#...#.....#.#.#",
      "#.#.#.###.#.#.#",
      "#.....#...#.#.#",
      "#.###.#.#.#.#.#",
      "#S..#.....#...#",
      "###############"
    ]
example2 =
  T.unlines
    [ "#################",
      "#...#...#...#..E#",
      "#.#.#.#.#.#.#.#.#",
      "#.#.#.#...#...#.#",
      "#.#.#.#.###.#.#.#",
      "#...#.#.#.....#.#",
      "#.#.#.#.#.#####.#",
      "#.#...#.#.#.....#",
      "#.#.#####.#.###.#",
      "#.#.#.......#...#",
      "#.#.###.#####.###",
      "#.#.#...#.....#.#",
      "#.#.#.#####.###.#",
      "#.#.#.........#.#",
      "#.#.#.#########.#",
      "#S#.............#",
      "#################"
    ]

spec :: Spec
spec = do
  describe "part 1" $ do
    it "examples" $ do
      part1 example1 `shouldBe` Just 7036
      part1 example2 `shouldBe` Just 11048
  describe "part 2" $ do
    it "examples" $ do
      part2 example1 `shouldBe` Just 45
      part2 example2 `shouldBe` Just 64
