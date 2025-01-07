-- |
-- Module:         Day8
-- Description:    <https://adventofcode.com/2024/day/8 Day 8: Resonant Collinearity>
module Day8 (part1, part2) where

import Control.Monad (guard)
import Data.Ix (Ix (inRange))
import Data.Map qualified as Map (elems, fromListWith)
import Data.Set qualified as Set (fromList, singleton, size, toList)
import Data.Text (Text)
import Data.Text qualified as T (length, lines, unpack)

solve :: ((Int, Int) -> (Int, Int) -> [(Int, Int)]) -> Text -> Int
solve extend input =
  Set.size . Set.fromList $ do
    values <- Set.toList <$> Map.elems points
    p0 <- values
    p1 <- values
    guard $ p0 /= p1
    takeWhile (inRange ((0, 0), (height - 1, width - 1))) $ extend p0 p1
  where
    input' = T.lines input
    height = length input'
    width = maximum $ T.length <$> input'
    points =
      Map.fromListWith (<>) $
        [ (c, Set.singleton (y, x))
        | (y, line) <- zip [0 ..] $ T.lines input,
          (x, c) <- zip [0 ..] $ T.unpack line,
          c /= '.'
        ]

part1, part2 :: Text -> Int
part1 = solve $ \(y0, x0) (y1, x1) -> [(2 * y1 - y0, 2 * x1 - x0)]
part2 = solve $ \(y0, x0) (y1, x1) -> zip [y1, 2 * y1 - y0 ..] [x1, 2 * x1 - x0 ..]
