{-# LANGUAGE OverloadedStrings #-}

module Day17Spec (spec) where

import Data.Text (Text)
import Data.Text qualified as T (unlines)
import Day17 (part1, part2, run, step)
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
      step [2, 6] (0, (-1, -1, 9)) `shouldBe` Just (Nothing, (2, (-1, 1, 9)))
      run [5, 0, 5, 1, 5, 4] (10, -1, -1) `shouldBe` [0, 1, 2]
      run [0, 1, 5, 4, 3, 0] (2024, -1, -1) `shouldBe` [4, 2, 5, 6, 7, 7, 7, 7, 3, 1, 0]
      step [1, 7] (0, (-1, 29, -1)) `shouldBe` Just (Nothing, (2, (-1, 26, -1)))
      step [4, 0] (0, (-1, 2024, 43690)) `shouldBe` Just (Nothing, (2, (-1, 44354, 43690)))
      part1 example1 `shouldBe` Right [4, 6, 3, 5, 6, 3, 5, 2, 1, 0]
  describe "part 2" $ do
    it "examples" $ do
      part2 example2 `shouldBe` Right 117440
