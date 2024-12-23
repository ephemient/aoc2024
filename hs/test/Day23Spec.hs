{-# LANGUAGE OverloadedStrings #-}

module Day23Spec (spec) where

import Data.Text (Text)
import Data.Text qualified as T (unlines)
import Day23 (part1, part2)
import Test.Hspec (Spec, describe, it, shouldBe)

example :: Text
example =
  T.unlines
    [ -- :r!pbpaste | sed 's/.*/  , "&"/;1s/,/ /'
      "kh-tc",
      "qp-kh",
      "de-cg",
      "ka-co",
      "yn-aq",
      "qp-ub",
      "cg-tb",
      "vc-aq",
      "tb-ka",
      "wh-tc",
      "yn-cg",
      "kh-ub",
      "ta-co",
      "de-co",
      "tc-td",
      "tb-wq",
      "wh-td",
      "ta-ka",
      "td-qp",
      "aq-cg",
      "wq-ub",
      "ub-vc",
      "de-ta",
      "wq-aq",
      "wq-vc",
      "wh-yn",
      "ka-de",
      "kh-ta",
      "co-tc",
      "wh-qp",
      "tb-vc",
      "td-yn"
    ]

spec :: Spec
spec = do
  describe "part 1" $ do
    it "examples" $ do
      part1 example `shouldBe` 7
  describe "part 2" $ do
    it "examples" $ do
      part2 example `shouldBe` "co,de,ka,ta"
