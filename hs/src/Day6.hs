-- |
-- Module:         Day6
-- Description:    <https://adventofcode.com/2024/day/6 Day 6: Guard Gallivant>
module Day6 (part1, part2) where

import Control.Monad (ap)
import Control.Parallel.Strategies (parMap, rseq)
import Data.Array.Unboxed (UArray, inRange, listArray, range, (!), (//))
import Data.Containers.ListUtils (nubOrd)
import Data.Ix (Ix)
import Data.Maybe (catMaybes, isJust)
import Data.Semigroup (Max (Max))
import Data.Set (Set)
import Data.Set qualified as Set (empty, insert, member, singleton)
import Data.Text (Text)
import Data.Text qualified as T (lines, unpack)

parse :: (Enum i, Ix i, Num i, Ord i) => Text -> (((i, i), (i, i)), Set (i, i), [((i, i), (i, i))])
parse input = (bounds, blocks, start)
  where
    (Max maxY, Max maxX, (blocks, start)) =
      foldl' (<>) (Max 0, Max 0, mempty) $
        [ ( Max y,
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
    bounds = ((0, 0), (maxY, maxX))

visited :: ((Int, Int), (Int, Int)) -> ((Int, Int) -> Bool) -> ((Int, Int), (Int, Int)) -> [((Int, Int), (Int, Int))]
visited bounds isBlock start = catMaybes $ takeWhile isJust $ iterate (>>= step) $ Just start
  where
    step (pos@(y, x), d@(dy, dx))
      | not $ inRange bounds pos' = Nothing
      | isBlock pos' = Just (pos, (dx, -dy))
      | otherwise = Just (pos', d)
      where
        pos' = (y + dy, x + dx)

part1 :: Text -> Int
part1 input = length $ nubOrd $ map fst $ start >>= visited bounds (`Set.member` blocks)
  where
    (bounds, blocks, start) = parse input

part2 :: Text -> Int
part2 input =
  length $ filter id $ start >>= (parMap rseq . isLoop) `ap` (nubOrd . map fst . visited bounds (blocks' !))
  where
    (bounds, blocks, start) = parse input
    blocks' = listArray @UArray bounds $ (`Set.member` blocks) <$> range bounds
    isLoop start' block = isLoop' 0 Set.empty $ visited bounds (blocks' // [(block, True)] !) start'
    isLoop' _ _ [] = False
    isLoop' (-1) seen ((_, (dy, _)) : rest) = isLoop' dy seen rest
    isLoop' _ seen ((pos, (-1, _)) : rest) = pos `Set.member` seen || isLoop' (-1) (Set.insert pos seen) rest
    isLoop' _ seen ((_, (dy, _)) : rest) = isLoop' dy seen rest
