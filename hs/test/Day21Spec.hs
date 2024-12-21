{-# LANGUAGE OverloadedStrings #-}

module Day21Spec (spec) where

import Data.Text (Text)
import Data.Text qualified as T (unlines)
import Day21 (solve)
import Test.Hspec (Spec, describe, it, shouldBe)

example :: Text
example =
  T.unlines
    [ "029A",
      "980A",
      "179A",
      "456A",
      "379A"
    ]

spec :: Spec
spec = do
  describe "part 1" $ do
    it "examples" $ do
      solve 2 example `shouldBe` 126384
