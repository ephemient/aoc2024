{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

-- |
-- Module:         Day6
-- Description:    <https://adventofcode.com/2024/day/6 Day 6: Guard Gallivant>
module Day6 (part1, part2) where

import Control.Monad (ap, foldM)
import Control.Monad.ST (ST, runST)
import Control.Parallel.Strategies (parMap, rseq)
import Data.Array.IArray ((!))
import Data.Array.MArray (newArray, writeArray)
import Data.Array.ST (STUArray)
import Data.Array.Unboxed (UArray)
import Data.Array.Unsafe (unsafeFreeze)
import Data.Functor (($>))
import Data.Ix (inRange)
import Data.Maybe (catMaybes, isJust)
import Data.Set qualified as Set (empty, fromList, insert, member, size, toList)
import Data.Text (Text)
import Data.Text qualified as T (length, lines, unpack)

parse :: Text -> (((Int, Int), (Int, Int)), UArray (Int, Int) Bool, [((Int, Int), (Int, Int))])
parse input = (bounds, blocks, start)
  where
    input' = T.lines input
    height = length input'
    width = foldl' max 0 $ T.length <$> input'
    bounds = ((0, 0), (height - 1, width - 1))
    (start, blocks) = runST $ do
      blocks' <- newArray bounds False :: ST s (STUArray s _ _)
      let go k (y, line) = foldM (go' y) k $ zip [0 ..] $ T.unpack line
          go' y k (x, '#') = writeArray blocks' (y, x) True $> k
          go' y k (x, '^') = pure $ ((y, x), (-1, 0)) : k
          go' y k (x, '<') = pure $ ((y, x), (0, -1)) : k
          go' y k (x, '>') = pure $ ((y, x), (0, 1)) : k
          go' y k (x, 'v') = pure $ ((y, x), (1, 0)) : k
          go' _ k _ = pure k
      (,) <$> foldM go [] (zip [0 ..] input') <*> unsafeFreeze blocks'

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
part1 input = Set.size $ Set.fromList $ map fst $ start >>= visited bounds (blocks !)
  where
    (bounds, blocks, start) = parse input

part2 :: Text -> Int
part2 input =
  length $ filter id $ start >>= (parMap rseq . isLoop) `ap` (Set.toList . Set.fromList . map fst . visited bounds (blocks !))
  where
    (bounds, blocks, start) = parse input
    isBlock block pos = pos == block || blocks ! pos
    isLoop start' block = isLoop' 0 Set.empty $ visited bounds (isBlock block) start'
    isLoop' _ _ [] = False
    isLoop' (-1) seen ((_, (dy, _)) : rest) = isLoop' dy seen rest
    isLoop' _ seen ((pos, (-1, _)) : rest) = pos `Set.member` seen || isLoop' (-1) (Set.insert pos seen) rest
    isLoop' _ seen ((_, (dy, _)) : rest) = isLoop' dy seen rest
