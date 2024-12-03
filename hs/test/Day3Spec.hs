{-# LANGUAGE OverloadedStrings #-}

module Day3Spec (spec) where

import Data.Text (Text)
import Data.Text qualified as T (unlines)
import Day3 (part1, part2)
import Test.Hspec (Spec, describe, it, shouldBe)

example1, example2 :: Text
example1 =
  T.unlines
    [ "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
    ]
example2 =
  T.unlines
    [ "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
    ]

spec :: Spec
spec = do
  describe "part 1" $ do
    it "examples" $ do
      part1 example1 `shouldBe` Right 161
  describe "part 2" $ do
    it "examples" $ do
      part2 example2 `shouldBe` Right 48
