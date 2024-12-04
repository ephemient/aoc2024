{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module:         Day4
-- Description:    <https://adventofcode.com/2024/day/4 Day 4: Ceres Search>
module Day4 (part1, part2) where

import Control.Monad (guard)
import Data.List (tails)
import Data.Text (Text)
import Data.Text qualified as T (drop, index, length, lines, reverse, splitOn, takeEnd, transpose, unpack)

part1 :: Text -> Int
part1 input = sum $ pred . length . T.splitOn "XMAS" <$> gs
  where
    g = T.lines input
    d1 = T.transpose (zipWith T.drop [0 ..] g) ++ T.transpose (zipWith T.takeEnd [0 ..] $ T.reverse <$> g)
    d2 = T.transpose (zipWith T.drop [0 ..] $ T.reverse <$> g) ++ T.transpose (zipWith T.takeEnd [0 ..] g)
    gs = concat [g, T.reverse <$> g, T.transpose g, T.transpose $ reverse g, d1, T.reverse <$> d1, d2, T.reverse <$> d2]

part2 :: Text -> Int
part2 input = length $ do
  prev : line : next : _ <- tails $ T.lines input
  (i, 'A') <- zip [0 ..] $ T.unpack line
  guard $ [prev !? pred i, prev !? succ i, next !? pred i, next !? succ i] `elem` ["MMSS", "MSMS", "SMSM", "SSMM"]
  where
    t !? i
      | 0 <= i, i < T.length t = t `T.index` i
      | otherwise = '\0'
