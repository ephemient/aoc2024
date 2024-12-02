-- |
-- Module:         Day2
-- Description:    <https://adventofcode.com/2024/day/2 Day 2: Red-Nosed Reports>
module Day2 (part1, part2) where

import Common (readEntire)
import Data.Ix (inRange)
import Data.List (inits, tails)
import Data.Text (Text)
import Data.Text qualified as T (lines, words)
import Data.Text.Read qualified as T (decimal)

parse :: Text -> Either String [[Int]]
parse = mapM (mapM (readEntire T.decimal) . T.words) . T.lines

isSafe, isSafe' :: [Int] -> Bool
isSafe report = all (inRange (-3, -1)) delta || all (inRange (1, 3)) delta
  where
    delta = zipWith (-) report $ drop 1 report
isSafe' report = any isSafe [a ++ b | (a, _ : b) <- zip (inits report) (tails report)]

part1 :: Text -> Either String Int
part1 input = length . filter isSafe <$> parse input

part2 :: Text -> Either String Int
part2 input = length . filter isSafe' <$> parse input
