-- |
-- Module:         Day10
-- Description:    <https://adventofcode.com/2024/day/10 Day 10: Hoof It>
module Day10 (part1, part2) where

import Data.Char (digitToInt, isDigit)
import Data.IntMap qualified as IntMap (findWithDefault, fromListWith)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map (Map)
import Data.Map qualified as Map (elems, fromListWith, fromSet, toList)
import Data.Monoid (Sum (Sum, getSum))
import Data.Set (Set)
import Data.Set qualified as Set (empty, member, singleton, size)
import Data.Text (Text)
import Data.Text qualified as T (lines, unpack)

parse :: Text -> NonEmpty (Set (Int, Int))
parse input = IntMap.findWithDefault Set.empty `flip` elevations <$> 0 :| [1 .. 9]
  where
    elevations =
      IntMap.fromListWith (<>) $
        [ (digitToInt c, Set.singleton (y, x))
        | (y, line) <- zip [0 ..] $ T.lines input,
          (x, c) <- zip [0 ..] $ T.unpack line,
          isDigit c
        ]

adj :: (Int, Int) -> [(Int, Int)]
adj (y, x) = [(y - 1, x), (y, x - 1), (y, x + 1), (y + 1, x)]

bfs :: (Semigroup a) => ((Int, Int) -> a) -> NonEmpty (Set (Int, Int)) -> Map (Int, Int) a
bfs start (zero :| elevations) = foldl bfs' (Map.fromSet start zero) elevations
  where
    bfs' acc points =
      Map.fromListWith (<>) $
        [ (q, m)
        | (p, m) <- Map.toList acc,
          q <- adj p,
          q `Set.member` points
        ]

part1 :: Text -> Int
part1 input = sum $ Set.size <$> bfs Set.singleton (parse input)

part2 :: Text -> Int
part2 input = getSum $ mconcat $ Map.elems $ bfs (const $ Sum 1) (parse input)
