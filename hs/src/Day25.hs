{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module:         Day25
-- Description:    <https://adventofcode.com/2024/day/25 Day 25: Code Chronicle>
module Day25 (part1) where

import Control.Arrow (Arrow ((&&&)))
import Data.List (partition)
import Data.List.NonEmpty (nonEmpty)
import Data.List.NonEmpty qualified as NonEmpty (head)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T (group, head, length, lines, splitOn, transpose)

part1 :: Text -> Int
part1 input =
  length
    [ ()
    | key <- keys,
      lock <- locks,
      and [k >= l | ((_, k), (_, l)) <- zip key lock]
    ]
  where
    stanzas = mapMaybe (fmap ((T.head &&& T.length) . NonEmpty.head) . nonEmpty . T.group) . T.transpose . T.lines <$> T.splitOn "\n\n" input
    (keys, locks) = partition (maybe False ((== '.') . fst . NonEmpty.head) . nonEmpty) stanzas
