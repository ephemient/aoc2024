{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module:         Day6
-- Description:    <https://adventofcode.com/2024/day/6 Day 6: Guard Gallivant>
module Day6 (part1, part2) where

import Control.Monad (ap)
import Control.Parallel.Strategies (parMap, rseq)
import Data.Containers.ListUtils (nubOrd)
import Data.List ((\\))
import Data.Maybe (catMaybes, isJust)
import Data.Set qualified as Set (empty, insert, member)
import Data.Text (Text)
import Data.Text qualified as T (concat, drop, index, length, lines, take, unpack)

visited :: [Text] -> (Int, Int) -> [((Int, Int), (Int, Int))]
visited g pos0 = catMaybes $ takeWhile isJust $ iterate (>>= step) $ Just (pos0, (-1, 0))
  where
    step (pos@(y, x), d@(dy, dx))
      | y' < 0 || length g <= y' || x' < 0 || T.length line <= x' = Nothing
      | line `T.index` x' == '#' = step (pos, (dx, -dy))
      | otherwise = Just ((y', x'), d)
      where
        y' = y + dy
        x' = x + dx
        line = g !! y'

part1 :: Text -> Int
part1 input = length $ nubOrd $ fst <$> visited g pos0
  where
    g = T.lines input
    [pos0] = [(y, x) | (y, line) <- zip [0 ..] g, (x, '^') <- zip [0 ..] $ T.unpack line]

part2 :: Text -> Int
part2 input =
  length . filter id . parMap rseq isLoop $
    [ above ++ T.concat [T.take x line, "#", T.drop (x + 1) line] : below
    | (y, x) <- nubOrd (fst <$> visited g pos0) \\ [pos0],
      let (above, line : below) = splitAt y g
    ]
  where
    g = T.lines input
    [pos0] = [(y, x) | (y, line) <- zip [0 ..] g, (x, '^') <- zip [0 ..] $ T.unpack line]
    isLoop g' = any (uncurry Set.member) $ zip `ap` scanl (flip Set.insert) Set.empty $ visited g' pos0
