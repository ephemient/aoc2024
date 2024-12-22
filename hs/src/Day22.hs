-- |
-- Module:         Day22
-- Description:    <https://adventofcode.com/2024/day/22 Day 22: Monkey Market>
module Day22 (part1, part2) where

import Common (readMany)
import Data.Bits (shiftL, shiftR, xor, (.&.))
import Data.IntMap qualified as IntMap (singleton, union)
import Data.List (tails)
import Data.Map qualified as Map (singleton, unionsWith)
import Data.Text (Text)
import Data.Text.Read qualified as T (decimal)

step :: Int -> Int
step num = num3
  where
    num1 = (num `xor` num `shiftL` 6) .&. 16777215
    num2 = (num1 `xor` num1 `shiftR` 5) .&. 16777215
    num3 = (num2 `xor` num2 `shiftL` 11) .&. 16777215

part1 :: Text -> Either String Int
part1 input = do
  (nums, _) <- readMany T.decimal input
  pure $ sum [iterate step num !! 2000 | num <- nums]

part2 :: Text -> Either String Int
part2 input = do
  (nums, _) <- readMany T.decimal input
  foldl' (flip $ max . Right . sum) (Left "empty") . Map.unionsWith IntMap.union $ do
    (i, num) <- zip [0 ..] nums
    let secrets = take 2001 $ iterate step num
        prices = map (`mod` 10) secrets
        deltas = zipWith (-) prices $ drop 1 prices
    (a : b : c : d : _, price) <- zip (tails deltas) $ drop 4 prices
    pure $ Map.singleton (a, b, c, d) $ IntMap.singleton i price
