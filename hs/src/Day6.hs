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
import Data.Set qualified as Set (empty, insert, member, singleton)
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
  length $ filter id $ start >>= (parMap rseq . isLoop) `ap` (nubOrd . map fst . visited maxes blocks)
  where
    (maxes, blocks, start) = parse input
    isLoop start' block = isLoop' 0 Set.empty $ visited maxes (Set.insert block blocks) start'
    isLoop' _ _ [] = False
    isLoop' (-1) seen ((_, (dy, _)) : rest) = isLoop' dy seen rest
    isLoop' _ seen ((pos, (-1, _)) : rest) = pos `Set.member` seen || isLoop' (-1) (Set.insert pos seen) rest
    isLoop' _ seen ((_, (dy, _)) : rest) = isLoop' dy seen rest
