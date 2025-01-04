-- |
-- Module:         Day20
-- Description:    <https://adventofcode.com/2024/day/20 Day 20: Race Condition>
module Day20 (solve) where

import Control.Parallel.Strategies (parMap, rseq)
import Data.List (tails)
import Data.Map qualified as Map (empty, member, size, toList)
import Data.Map.Strict qualified as Map (insert)
import Data.Text (Text)
import Data.Text qualified as T (index, length, lines, unpack)
import Data.Vector qualified as V (fromList, length, (!))

solve :: Int -> Int -> Text -> Int
solve cheats time input =
  sum . parMap rseq length $
    [ [ ()
      | ((y2, x2), j) <- takeWhile ((<= (y1 + cheats, x1)) . fst) rest,
        let distance = abs (y2 - y1) + abs (x2 - x1),
        distance <= cheats && distance + time <= abs (j - i)
      ]
    | y0 <- [0 .. V.length grid - 1],
      (x0, 'S') <- zip [0 ..] . T.unpack $ grid V.! y0,
      ((y1, x1), i) : rest <- paths Map.empty (y0, x0) >>= tails . Map.toList
    ]
  where
    grid = V.fromList $ T.lines input
    paths path pos@(y, x)
      | y < 0 || V.length grid <= y = []
      | x < 0 || T.length line <= x = []
      | '#' <- line `T.index` x = []
      | 'E' <- line `T.index` x = [path']
      | pos `Map.member` path = []
      | otherwise = concatMap (paths path') [(y - 1, x), (y, x - 1), (y, x + 1), (y + 1, x)]
      where
        line = grid V.! y
        path' = Map.insert pos (Map.size path) path
