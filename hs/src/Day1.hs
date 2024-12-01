{-|
Module:         Day1
Description:    <https://adventofcode.com/2024/day/1 Day 1: Historian Hysteria>
-}
module Day1 (part1, part2) where

import Data.Function (on)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap (fromListWith, findWithDefault)
import Data.List (sort, transpose)
import Data.Text (Text)
import qualified Data.Text as T (lines, words, unpack)

part1 :: Text -> Int
part1 input = sum $ abs <$> cs where
    [as, bs] = transpose $ map (map (read . T.unpack) . T.words) $ T.lines input
    cs = (zipWith (-) `on` sort) as bs

part2 :: Text -> Int
part2 input = sum [a * IntMap.findWithDefault 0 a cs | a <- as] where
    [as, bs] = transpose $ map (map (read . T.unpack) . T.words) $ T.lines input
    cs = IntMap.fromListWith (+) [(b, 1) | b <- bs]
