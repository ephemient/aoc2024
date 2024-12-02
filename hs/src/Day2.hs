-- |
-- Module:         Day2
-- Description:    <https://adventofcode.com/2024/day/2 Day 2: Red-Nosed Reports>
module Day2 (part1, part2) where

import Common (readEntire)
import Control.Monad (ap, foldM_, guard)
import Data.Functor (($>))
import Data.List (inits, tails)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T (lines, words)
import Data.Text.Read qualified as T (decimal)

parse :: Text -> Either String [[Int]]
parse = mapM (mapM (readEntire T.decimal) . T.words) . T.lines

isSafe1, isSafe2 :: [Int] -> Bool
isSafe1 = isJust . foldM_ go EQ . (zipWith (-) `ap` drop 1)
  where
    go k x = guard (x /= 0 && abs x <= 3 && k /= compare 0 x) $> compare x 0
isSafe2 report = any isSafe1 [a ++ b | (a, _ : b) <- zip (inits report) (tails report)]

part1 :: Text -> Either String Int
part1 input = length . filter isSafe1 <$> parse input

part2 :: Text -> Either String Int
part2 input = length . filter isSafe2 <$> parse input
