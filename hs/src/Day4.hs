{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module:         Day4
-- Description:    <https://adventofcode.com/2024/day/4 Day 4: Ceres Search>
module Day4 (part1, part2) where

import Control.Monad (guard)
import Data.List (tails)
import Data.Text (Text)
import Data.Text qualified as T (drop, lines, reverse, splitAt, transpose, unpack, zip)
import Data.Text.Internal.Search qualified as T (indices)

part1 :: Text -> Int
part1 input = sum $ countXmas <$> grid ++ T.transpose grid ++ concat diagonals
  where
    grid = T.lines input
    diagonals = do
      (lower, upper) <- unzip . zipWith T.splitAt [0 ..] <$> [grid, reverse grid]
      T.transpose <$> [T.reverse <$> lower, upper]
    countXmas line = length (T.indices "XMAS" line) + length (T.indices "SAMX" line)

part2 :: Text -> Int
part2 input = length $ do
  prev : line : next : _ <- tails $ T.lines input
  ('A', x, y) <- zip3 (T.unpack $ T.drop 1 line) (T.zip prev $ T.drop 2 next) (T.zip next $ T.drop 2 prev)
  guard $ ok x && ok y
  where
    ok ('M', 'S') = True
    ok ('S', 'M') = True
    ok _ = False
