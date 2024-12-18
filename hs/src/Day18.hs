{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module:         Day18
-- Description:    <https://adventofcode.com/2024/day/18 Day 18: RAM Run>
module Day18 (part1, part1', part2, part2') where

import Common (readEntire)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty (cons, toList)
import Data.Set (Set)
import Data.Set qualified as Set (empty, fromList, insert, member, notMember)
import Data.Text (Text)
import Data.Text qualified as T (lines, stripPrefix)
import Data.Text.Read (Reader)
import Data.Text.Read qualified as T (decimal)

coord :: (Integral a) => Reader (a, a)
coord input = do
  (x, input1) <- T.decimal input
  input2 <- maybe (Left "missing comma") Right $ T.stripPrefix "," input1
  (y, input3) <- T.decimal input2
  pure ((x, y), input3)

part1 :: Text -> Either String Int
part1 = part1' 70 1024

part1' :: Int -> Int -> Text -> Either String Int
part1' size n input = do
  coords <- mapM (readEntire coord) . take n $ T.lines input
  case go size $ Set.fromList coords of
    Just path -> Right $ length path - 1
    Nothing -> Left "no solution"

go :: Int -> Set (Int, Int) -> Maybe (NonEmpty (Int, Int))
go size visited = go' visited [(0, 0) :| []] []
  where
    go' visited' (path@(pos@(x, y) :| _) : queue1) queue2
      | pos `Set.member` visited' = go' visited' queue1 queue2
      | pos == (size, size) = Just path
      | otherwise =
          go' (Set.insert pos visited') queue1 $
            [ NonEmpty.cons pos' path
            | pos'@(x', y') <- [(x - 1, y), (x, y - 1), (x, y + 1), (x + 1, y)],
              0 <= x' && x' <= size && 0 <= y' && y' <= size
            ]
              ++ queue2
    go' _ _ [] = Nothing
    go' visited' [] queue2 = go' visited' (reverse queue2) []

part2 :: Text -> Either String (Int, Int)
part2 = part2' 70

part2' :: Int -> Text -> Either String (Int, Int)
part2' size input = mapM (readEntire coord) (T.lines input) >>= go' Set.empty
  where
    go' visited (candidate : rest) =
      case go size visited' of
        Just path ->
          let path' = Set.fromList $ NonEmpty.toList path
              (skip, rest') = span (`Set.notMember` path') rest
           in go' (visited' <> Set.fromList skip) rest'
        Nothing -> Right candidate
      where
        visited' = Set.insert candidate visited
    go' _ _ = Left "no solution"
