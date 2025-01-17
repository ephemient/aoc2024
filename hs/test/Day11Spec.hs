{-# LANGUAGE OverloadedStrings #-}

module Day11Spec (spec) where

import Data.Text (Text)
import Day11 (solve)
import Test.Hspec (Spec, describe, it, shouldReturn)

example1, example2 :: Text
example1 = "0 1 10 99 999\n"
example2 = "125 17\n"

spec :: Spec
spec = do
  describe "part 1" $ do
    it "examples" $ do
      solve 1 example1 `shouldReturn` 7
      solve 6 example2 `shouldReturn` 22
      solve 25 example2 `shouldReturn` 55312
