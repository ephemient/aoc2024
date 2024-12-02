-- |
-- Module:         Day1
-- Description:    <https://adventofcode.com/2024/day/1 Day 1: Historian Hysteria>
module Day1 (part1, part2) where

import Common (readEntire)
import Data.Either (fromLeft)
import Data.Function (on)
import Data.IntMap qualified as IntMap (findWithDefault)
import Data.IntMap.Strict qualified as IntMap (fromListWith)
import Data.List (sort, transpose)
import Data.Text (Text)
import Data.Text qualified as T (lines, words)
import Data.Text.Read qualified as T (decimal)

parse :: Text -> Either String [[Int]]
parse = fmap transpose . mapM (mapM (readEntire T.decimal) . T.words) . T.lines

part1 :: Text -> Either String Int
part1 input = case parse input of
  Right [as, bs] -> pure $ sum $ abs <$> (zipWith (-) `on` sort) as bs
  other -> Left $ fromLeft "no parse" other

part2 :: Text -> Either String Int
part2 input = case parse input of
  Right [as, bs] ->
    let cs = IntMap.fromListWith (+) [(b, 1) | b <- bs]
     in pure $ sum [a * IntMap.findWithDefault 0 a cs | a <- as]
  other -> Left $ fromLeft "no parse" other
