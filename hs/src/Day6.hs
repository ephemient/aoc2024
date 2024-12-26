-- |
-- Module:         Day6
-- Description:    <https://adventofcode.com/2024/day/6 Day 6: Guard Gallivant>
module Day6 (part1, part2) where

import Control.Monad (ap)
import Control.Parallel.Strategies (parList, rseq, withStrategy)
import Data.Containers.ListUtils (nubOrd)
import Data.List ((\\))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (catMaybes, isJust)
import Data.Semigroup (Max (Max), sconcat)
import Data.Set (Set)
import Data.Set qualified as Set (empty, insert, member, singleton)
import Data.Text (Text)
import Data.Text qualified as T (lines, unpack)

parse :: Text -> ((Int, Int), Set (Int, Int), [(Int, Int)])
parse input = ((maxY, maxX), blocks, start)
  where
    (Max maxY, Max maxX, (blocks, start)) =
      sconcat $
        (Max 0, Max 0, mempty)
          :| [ ( Max y,
                 Max x,
                 case char of
                   '^' -> (mempty, [(y, x)])
                   '#' -> (Set.singleton (y, x), mempty)
                   _ -> mempty
               )
             | (y, line) <- zip [0 ..] $ T.lines input,
               (x, char) <- zip [0 ..] $ T.unpack line
             ]

visited :: (Int, Int) -> Set (Int, Int) -> (Int, Int) -> [((Int, Int), (Int, Int))]
visited (maxY, maxX) blocks start = catMaybes $ takeWhile isJust $ iterate (>>= step) $ Just (start, (-1, 0))
  where
    step (pos@(y, x), d@(dy, dx))
      | y' < 0 || maxY < y' || x' < 0 || maxX < x' = Nothing
      | (y', x') `Set.member` blocks = step (pos, (dx, -dy))
      | otherwise = Just ((y', x'), d)
      where
        y' = y + dy
        x' = x + dx

part1 :: Text -> Int
part1 input = length $ nubOrd $ map fst $ start >>= visited maxes blocks
  where
    (maxes, blocks, start) = parse input

part2 :: Text -> Int
part2 input =
  length . filter id . withStrategy (parList rseq) $
    [ isLoop (Set.insert add blocks) pos0
    | pos0 <- start,
      add <- nubOrd (fst <$> visited maxes blocks pos0) \\ [pos0]
    ]
  where
    (maxes, blocks, start) = parse input
    isLoop blocks' pos0 = any (uncurry Set.member) $ zip `ap` scanl (flip Set.insert) Set.empty $ visited maxes blocks' pos0
