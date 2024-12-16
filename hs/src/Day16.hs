{-# LANGUAGE ViewPatterns #-}

-- |
-- Module:         Day16
-- Description:    <https://adventofcode.com/2024/day/16 Day 16: Reindeer Maze>
module Day16 (part1, part2) where

import Control.Arrow (second)
import Control.Exception (assert)
import Data.Heap (FstMinPolicy)
import Data.Heap qualified as Heap (insert, singleton, view)
import Data.Map qualified as Map (empty, insert, (!?))
import Data.Set (Set)
import Data.Set qualified as Set (empty, fromList, insert, member, notMember, size)
import Data.Text (Text)
import Data.Text qualified as T (foldl)

parse :: Text -> Maybe (Set (Int, Int), (Int, Int), (Int, Int))
parse input = case T.foldl parse' (0, 0, Set.empty, [], []) input of
  (_, _, maze, [start], [end]) -> Just (maze, start, end)
  _ -> Nothing
  where
    parse' (y, _, maze, start, end) '\n' = (y + 1, 0, maze, start, end)
    parse' (y, x, maze, start, end) '#' = (y, x + 1, Set.insert (y, x) maze, start, end)
    parse' (y, x, maze, start, end) 'S' = (y, x + 1, maze, (y, x) : start, end)
    parse' (y, x, maze, start, end) 'E' = (y, x + 1, maze, start, (y, x) : end)
    parse' (y, x, maze, start, end) _ = (y, x + 1, maze, start, end)

part1 :: Text -> Maybe Int
part1 input = do
  (maze, start, end) <- parse input
  let go visited (Heap.view -> Just ((score, pv@(p@(y, x), v@(dy, dx))), queue'))
        | p == end = Just score
        | pv `Set.member` visited = go visited queue'
        | otherwise =
            go (Set.insert pv visited) . foldl' (flip Heap.insert) queue' $
              [(score + 1, (p', v)) | let p' = (y + dy, x + dx), p' `Set.notMember` maze]
                ++ [(score + 1000, (p, (-dx, dy))), (score + 1000, (p, (dx, -dy)))]
      go _ _ = Nothing
  go Set.empty $ Heap.singleton @FstMinPolicy (0 :: Int, (start, (0, 1)))

part2 :: Text -> Maybe Int
part2 input = do
  (maze, start, end) <- parse input
  best <- part1 input
  let go acc visited (Heap.view -> Just ((score, (pv@(p@(y, x), v@(dy, dx)), path)), queue))
        | p == end = assert (score == best) $ go (acc <> Set.fromList (p : path)) visited' queue
        | Just score' <- visited Map.!? pv, score' < score = go acc visited queue
        | otherwise =
            go acc visited' . foldl' (flip Heap.insert) queue . map (second (,path')) . filter ok $
              [(score + 1, (p', v)) | let p' = (y + dy, x + dx), p' `Set.notMember` maze]
                ++ [(score + 1000, (p, (-dx, dy))), (score + 1000, (p, (dx, -dy)))]
        where
          visited' = Map.insert pv score visited
          path' = p : path
          ok (score', pv') = score' <= best && maybe True (>= score) (visited Map.!? pv')
      go acc _ _ = Set.size acc
  pure $ go Set.empty Map.empty $ Heap.singleton @FstMinPolicy (0 :: Int, ((start, (0, 1)), []))
