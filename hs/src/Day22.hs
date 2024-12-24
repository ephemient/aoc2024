-- |
-- Module:         Day22
-- Description:    <https://adventofcode.com/2024/day/22 Day 22: Monkey Market>
module Day22 (part1, part2) where

import Common (readMany)
import Control.Parallel.Strategies (parList, rseq, withStrategy)
import Data.Bits (shiftL, shiftR, xor, (.&.))
import Data.IntMap.Strict qualified as IntMap (fromListWith, null, unionsWith)
import Data.List (tails)
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
  pure $ sum $ withStrategy (parList rseq) [iterate step num !! 2000 | num <- nums]

part2 :: Text -> Either String Int
part2 input = do
  (nums, _) <- readMany T.decimal input
  let results =
        IntMap.unionsWith (+) . withStrategy (parList rseq) $
          [ IntMap.fromListWith (const id) $
              [ (((a * 19 + b) * 19 + c) * 19 + d, price)
              | (a : b : c : d : _, price) <- zip (tails deltas) $ drop 4 prices
              ]
          | num <- nums,
            let secrets = take 2001 $ iterate step num
                prices = map (`mod` 10) secrets
                deltas = zipWith (-) prices $ drop 1 prices
          ]
  pure $ if IntMap.null results then 0 else maximum results