{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module:         Day19
-- Description:    <https://adventofcode.com/2024/day/19 Day 19: Linen Layout>
module Day19 (solve) where

import Data.Bifunctor (bimap)
import Data.Foldable (foldMap')
import Data.Monoid (Sum (Sum, getSum))
import Data.Text (Text)
import Data.Text qualified as T (isSuffixOf, length, lines, null, splitOn, take)
import Data.Vector qualified as V (generate, (!))

count :: [Text] -> Text -> Int
count keys target = getCount $ T.length target
  where
    counts = V.generate (T.length target) getCount
    getCount 0 = 1
    getCount i = sum [counts V.! (i - T.length key) | key <- keys, key `T.isSuffixOf` T.take i target]

solve :: Text -> (Int, Int)
solve input
  | keys : rest <- T.lines input =
      bimap getSum getSum . foldMap' ((Sum 1,) . Sum) . filter (> 0) $ count (T.splitOn ", " keys) <$> filter (not . T.null) rest
  | otherwise = (0, 0)
