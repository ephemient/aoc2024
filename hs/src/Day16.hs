{-# LANGUAGE ViewPatterns #-}

-- |
-- Module:         Day16
-- Description:    <https://adventofcode.com/2024/day/16 Day 16: Reindeer Maze>
module Day16 (part1, part2) where

import Data.Bits ((.&.), (.|.))
import Data.Heap (FstMinPolicy)
import Data.Heap qualified as Heap (insert, singleton, view)
import Data.List (find, mapAccumL)
import Data.Map qualified as Map (empty, findWithDefault, insert, (!?))
import Data.Map.Strict qualified as Map (fromListWith)
import Data.Set (Set)
import Data.Set qualified as Set (elems, empty, fromList, insert, notMember)
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

walk :: Set (Int, Int) -> (Int, Int) -> [(Int, (((Int, Int), (Int, Int)), Int))]
walk maze start = walk' Map.empty $ Heap.singleton @FstMinPolicy (0, ((start, (0, 1)), 0))
  where
    walk' visited (Heap.view -> Just (state@(score, (pv@((y, x), v@(dy, dx)), _)), queue))
      | Just score' <- visited Map.!? pv = case compare score score' of
          LT -> error "invariants violated"
          EQ -> state : walk' visited queue
          GT -> walk' visited queue
      | otherwise = state : walk' (Map.insert pv score visited) queue'
      where
        queue' =
          foldl' (flip Heap.insert) queue . filter ((`Set.notMember` maze) . fst . fst . snd) $
            [ (score + 1, (((y + dy, x + dx), v), 1)),
              (score + 1001, (((y - dx, x + dy), (-dx, dy)), 2)),
              (score + 1001, (((y + dx, x - dy), (dx, -dy)), 4))
            ]
    walk' _ _ = []

part1 :: Text -> Maybe Int
part1 input = do
  (maze, start, end) <- parse input
  fmap fst . find ((==) end . fst . fst . snd) $ walk maze start

part2 :: Text -> Maybe Int
part2 input = do
  (maze, start, end) <- parse input
  let go1 best ((score, state@((p, _), _)) : rest)
        | Just best' <- best, best' < score = []
        | otherwise = state : go1 (if p == end then Just score else best) rest
      go1 _ _ = []
      paths = Map.fromListWith (.|.) $ go1 Nothing $ walk maze start
      go2 visited (pv@((y, x), v@(dy, dx)) : queue) queue' =
        go2 (foldl' (flip Set.insert) visited next) queue $ next ++ queue'
        where
          ways = Map.findWithDefault 0 pv paths
          next =
            filter (`Set.notMember` visited) . map ((y - dy, x - dx),) $
              [v | ways .&. 1 /= 0] ++ [(dx, -dy) | ways .&. 2 /= 0] ++ [(-dx, dy) | ways .&. 4 /= 0]
      go2 visited _ [] = visited
      go2 visited _ queue = go2 visited (reverse queue) []
      goals = [(end, v) | v <- [(0, 1), (1, 0), (0, -1), (-1, 0)]]
      f prev (cur, _) = (Just cur, Just cur /= prev)
  pure $ length $ filter id $ snd $ mapAccumL f Nothing $ Set.elems $ go2 (Set.fromList goals) goals []
