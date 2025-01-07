{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module:         Day19
-- Description:    <https://adventofcode.com/2024/day/19 Day 19: Linen Layout>
module Day19 (solve) where

import Control.Parallel.Strategies
import Data.Foldable (foldMap')
import Data.Monoid (Sum (Sum))
import Data.Text (Text)
import Data.Text qualified as T (lines, null, splitOn)
import Data.Text.Array qualified as A (equal)
import Data.Text.Internal (Text (Text))
import Data.Text.Unsafe qualified as T (lengthWord8)
import Data.Vector qualified as V (generate, (!))

count :: [Text] -> Text -> Int
count keys target = getCount $ T.lengthWord8 target
  where
    counts = V.generate (T.lengthWord8 target) getCount
    getCount 0 = 1
    getCount i = sum [counts V.! (i - T.lengthWord8 key) | key <- keys, key `isSuffixOfAt` i $ target]

isSuffixOfAt :: Text -> Int -> Text -> Bool
isSuffixOfAt (Text a aoff alen) n (Text b boff blen) =
  alen <= n && n <= blen && A.equal a aoff b (boff + n - alen) alen

solve :: Text -> Maybe (Int, Int)
solve (T.lines -> (T.splitOn ", " -> keys) : rest) | not $ any T.null keys = Just (part1, part2)
  where
    (Sum part1, Sum part2) = foldMap' ((Sum 1,) . Sum) . filter (> 0) . parMap rseq (count keys) $ filter (not . T.null) rest
solve _ = Nothing
