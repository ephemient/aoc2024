-- |
-- Module:         Day6
-- Description:    <https://adventofcode.com/2024/day/6 Day 6: Guard Gallivant>
module Day6 (part1, part2) where

import Control.Monad (ap)
import Control.Parallel.Strategies (parMap, rseq)
import Data.Containers.ListUtils (nubOrd)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (catMaybes, isJust)
import Data.Semigroup (Max (Max), sconcat)
import Data.Set (Set)
import Data.Set qualified as Set (empty, insert, lookupGE, member, notMember, singleton)
import Data.Text (Text)
import Data.Text qualified as T (lines, unpack)

parse :: Text -> ((Int, Int), Set (Int, Int), [((Int, Int), (Int, Int))])
parse input = ((maxY, maxX), blocks, start)
  where
    (Max maxY, Max maxX, (blocks, start)) =
      sconcat $
        (Max 0, Max 0, mempty)
          :| [ ( Max y,
                 Max x,
                 case char of
                   '^' -> (mempty, [((y, x), (-1, 0))])
                   '<' -> (mempty, [((y, x), (0, -1))])
                   '>' -> (mempty, [((y, x), (0, 1))])
                   'v' -> (mempty, [((y, x), (1, 0))])
                   '#' -> (Set.singleton (y, x), mempty)
                   _ -> mempty
               )
             | (y, line) <- zip [0 ..] $ T.lines input,
               (x, char) <- zip [0 ..] $ T.unpack line
             ]

visited :: (Int, Int) -> Set (Int, Int) -> ((Int, Int), (Int, Int)) -> [((Int, Int), (Int, Int))]
visited (maxY, maxX) blocks start = catMaybes $ takeWhile isJust $ iterate (>>= step) $ Just start
  where
    step (pos@(y, x), d@(dy, dx))
      | y' < 0 || maxY < y' || x' < 0 || maxX < x' = Nothing
      | (y', x') `Set.member` blocks = Just (pos, (dx, -dy))
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
  length . filter id . parMap rseq isLoop $
    start >>= (zip `ap` scanl (flip Set.insert) Set.empty) . visited maxes blocks
  where
    (maxes, blocks, start) = parse input
    isLoop (start'@((y, x), (dy, dx)), seen) =
      pos' `Set.notMember` blocks
        && Just pos' /= (fst <$> Set.lookupGE (pos', minBound) seen)
        && or (zipWith Set.member `ap` scanl (flip Set.insert) seen $ visited maxes blocks' start')
      where
        pos' = (y + dy, x + dx)
        blocks' = Set.insert pos' blocks
