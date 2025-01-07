-- |
-- Module:         Day20
-- Description:    <https://adventofcode.com/2024/day/20 Day 20: Race Condition>
module Day20 (solve) where

import Control.Parallel.Strategies (parMap, rseq)
import Data.List (sort, tails)
import Data.Maybe (listToMaybe)
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
      ((y1, x1), i) : rest <- take 1 (paths 0 [] (y0, x0)) >>= tails . sort
    ]
  where
    grid = V.fromList $ T.lines input
    paths n path pos@(y, x)
      | y < 0 || V.length grid <= y = []
      | x < 0 || T.length line <= x = []
      | '#' <- line `T.index` x = []
      | 'E' <- line `T.index` x = [path']
      | otherwise =
          filter
            (maybe (const True) ((/=) . fst) $ listToMaybe path)
            [(y - 1, x), (y, x - 1), (y, x + 1), (y + 1, x)]
            >>= paths (n + 1) path'
      where
        line = grid V.! y
        path' = (pos, n) : path
