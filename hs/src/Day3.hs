{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- |
-- Module:         Day3
-- Description:    <https://adventofcode.com/2024/day/3 Day 3: Mull It Over>
module Day3 (part1, part2) where

import Data.Either (rights)
import Data.Text (Text)
import Data.Text qualified as T (splitOn, pattern (:<))
import Data.Text.Read qualified as T (decimal)

part1 :: Text -> Int
part1 input =
  sum
    [ a * b
    | part <- drop 1 $ T.splitOn "mul(" input,
      (a, ',' T.:< part') <- rights [T.decimal part],
      (b, ')' T.:< _) <- rights [T.decimal part']
    ]

part2 :: Text -> Int
part2 input = sum $ part1 <$> (T.splitOn "do()" input >>= take 1 . T.splitOn "don't()")
