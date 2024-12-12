{-# LANGUAGE ViewPatterns #-}

-- |
-- Module:         Day12
-- Description:    <https://adventofcode.com/2024/day/12 Day 12: Garden Groups>
module Day12 (part1, part2) where

import Control.Arrow (first)
import Control.Monad (ap)
import Data.Either (partitionEithers)
import Data.Function (on)
import Data.List (groupBy)
import Data.Map qualified as Map (delete, filter, fromDistinctAscList, keys, minViewWithKey, size, (!?))
import Data.Map.Strict qualified as Map (fromListWith)
import Data.Semigroup (Arg (Arg))
import Data.Text (Text)
import Data.Text qualified as T (lines, unpack)

solve :: ([(Int, Int)] -> Int) -> Text -> Int
solve perimeter input =
  go 0 . Map.fromDistinctAscList $
    [ ((y, x), c)
    | (y, line) <- zip [0 ..] $ T.lines input,
      (x, c) <- zip [0 ..] $ T.unpack line
    ]
  where
    go !cost plots@(Map.minViewWithKey -> Just ((pos, c), _)) =
      let (points, plots') = dfs pos c plots
       in go (cost + length points * perimeter points) plots'
    go cost _ = cost
    dfs pos@(y, x) c plots
      | plots Map.!? pos /= Just c = ([], plots)
      | otherwise = (pos : points1 ++ points2 ++ points3 ++ points4, plots4)
      where
        plots0 = Map.delete pos plots
        (points1, plots1) = dfs (y - 1, x) c plots0
        (points2, plots2) = dfs (y, x - 1) c plots1
        (points3, plots3) = dfs (y, x + 1) c plots2
        (points4, plots4) = dfs (y + 1, x) c plots3

part1 :: Text -> Int
part1 = solve $ \points ->
  Map.size . Map.filter (== 1) . Map.fromListWith (+) . map (,1 :: Int) $ do
    (y, x) <- points
    [Left (y, x), Right (x, y), Right (x + 1, y), Left (y + 1, x)]

part2 :: Text -> Int
part2 = solve $ \points ->
  let getArg (Arg (Left a) b) = Left (a, b)
      getArg (Arg (Right a) b) = Right (a, b)
      (horizontalEdges, verticalEdges) =
        partitionEithers . map getArg . Map.keys . Map.filter (== 1) . Map.fromListWith (+) $ do
          (y, x) <- points
          (,1 :: Int)
            <$> [ Arg (Left (y, x)) True,
                  Arg (Right (x, y)) False,
                  Arg (Right (x + 1, y)) True,
                  Arg (Left (y + 1, x)) False
                ]
      countConsecutive = succ . length . filter not . (zipWith ok `ap` drop 1)
      ok (a, b) (c, d) = abs (c - a) <= 1 && b == d
   in sum
        [ countConsecutive $ first snd <$> edges
        | edges <-
            groupBy ((==) `on` fst . fst) horizontalEdges
              ++ groupBy ((==) `on` fst . fst) verticalEdges
        ]
