{-# LANGUAGE MultiWayIf #-}

-- |
-- Module:         Day11
-- Description:    <https://adventofcode.com/2024/day/11 Day 11: Plutonian Pebbles>
module Day11 (part1, part2, solve) where

import Common (readEntire)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap (toList)
import Data.IntMap.Strict qualified as IntMap (fromListWith)
import Data.Text (Text)
import Data.Text qualified as T (words)
import Data.Text.Read qualified as T (decimal)

part1, part2 :: Text -> Either String Int
part1 = solve 25
part2 = solve 75

solve :: Int -> Text -> Either String Int
solve n input = do
  nums <- mapM (readEntire T.decimal) $ T.words input
  pure $ foldl' (+) 0 $ iterate step (IntMap.fromListWith (+) $ (,1) <$> nums) !! n

step :: IntMap Int -> IntMap Int
step counts = IntMap.fromListWith (+) $ do
  (x, n) <- IntMap.toList counts
  if
    | x == 0 -> [(1, n)]
    | s <- show x, (l, 0) <- length s `divMod` 2, (a, b) <- splitAt l s -> [(read a, n), (read b, n)]
    | otherwise -> [(2024 * x, n)]
