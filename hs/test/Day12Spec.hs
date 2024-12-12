{-# LANGUAGE OverloadedStrings #-}

module Day12Spec (spec) where

import Data.Text (Text)
import Data.Text qualified as T (unlines)
import Day12 (part1, part2)
import Test.Hspec (Spec, describe, it, shouldBe)

example1, example2, example3, example4, example5 :: Text
example1 =
  T.unlines
    [ "AAAA",
      "BBCD",
      "BBCC",
      "EEEC"
    ]
example2 =
  T.unlines
    [ "OOOOO",
      "OXOXO",
      "OOOOO",
      "OXOXO",
      "OOOOO"
    ]
example3 =
  T.unlines
    [ "RRRRIICCFF",
      "RRRRIICCCF",
      "VVRRRCCFFF",
      "VVRCCCJFFF",
      "VVVVCJJCFE",
      "VVIVCCJJEE",
      "VVIIICJJEE",
      "MIIIIIJJEE",
      "MIIISIJEEE",
      "MMMISSJEEE"
    ]
example4 =
  T.unlines
    [ "EEEEE",
      "EXXXX",
      "EEEEE",
      "EXXXX",
      "EEEEE"
    ]
example5 =
  T.unlines
    [ "AAAAAA",
      "AAABBA",
      "AAABBA",
      "ABBAAA",
      "ABBAAA",
      "AAAAAA"
    ]

spec :: Spec
spec = do
  describe "part 1" $ do
    it "examples" $ do
      part1 example1 `shouldBe` 140
      part1 example2 `shouldBe` 772
      part1 example3 `shouldBe` 1930
  describe "part 2" $ do
    it "examples" $ do
      part2 example1 `shouldBe` 80
      part2 example2 `shouldBe` 436
      part2 example4 `shouldBe` 236
      part2 example5 `shouldBe` 368
      part2 example3 `shouldBe` 1206
