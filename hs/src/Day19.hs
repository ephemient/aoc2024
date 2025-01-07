{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module:         Day19
-- Description:    <https://adventofcode.com/2024/day/19 Day 19: Linen Layout>
module Day19 (solve) where

import Control.Parallel.Strategies (parMap, rseq)
import Data.Foldable (foldMap')
import Data.Monoid (Sum (Sum))
import Data.Text (Text)
import Data.Text qualified as T (isSuffixOf, lines, null, splitOn)
import Data.Text.Unsafe qualified as T (lengthWord8, takeWord8)
import Data.Vector qualified as V (generate, (!))

count :: [Text] -> Text -> Int
count keys target = getCount $ T.lengthWord8 target
  where
    counts = V.generate (T.lengthWord8 target) getCount
    getCount 0 = 1
    getCount i = sum [counts V.! (i - T.lengthWord8 key) | key <- keys, key `T.isSuffixOf` T.takeWord8 i target]

solve :: Text -> Maybe (Int, Int)
solve (T.lines -> (T.splitOn ", " -> keys) : rest) | not $ any T.null keys = Just (part1, part2)
  where
    (Sum part1, Sum part2) = foldMap' ((Sum 1,) . Sum) . filter (> 0) . parMap rseq (count keys) $ filter (not . T.null) rest
solve _ = Nothing
