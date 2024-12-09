-- |
-- Module:         Day9
-- Description:    <https://adventofcode.com/2024/day/9 Day 9: Disk Defragmenter>
module Day9 (part1, part2) where

import Control.Monad (ap)
import Control.Monad.ST (runST)
import Data.Bits (clearBit)
import Data.Char (digitToInt, isDigit)
import Data.Text (Text)
import Data.Text qualified as T (unpack)
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as V (fromList, thaw)
import Data.Vector.Unboxed.Mutable qualified as MV (length, read, write)

parse :: Text -> Vector (Int, Int)
parse = V.fromList . (zip `ap` scanl (+) 0) . map digitToInt . filter isDigit . T.unpack

triRange :: (Integral a) => a -> a -> a
triRange offset size = (2 * offset + size - 1) * size `div` 2

part1 :: Text -> Int
part1 input = runST $ do
  chunks <- V.thaw $ parse input
  let go i j k
        | i > j = pure k
        | even i = do
            (size, offset) <- MV.read chunks i
            go (i + 1) j $! k + i `div` 2 * triRange offset size
        | otherwise = do
            (freeSize, freeOffset) <- MV.read chunks i
            (size, offset) <- MV.read chunks j
            let usedSize = min freeSize size
            MV.write chunks i (freeSize - usedSize, freeOffset + usedSize)
            MV.write chunks j (size - usedSize, offset)
            go (if freeSize <= size then i + 1 else i) (if freeSize >= size then j - 2 else j) $!
              k + j `div` 2 * triRange freeOffset usedSize
  go 0 (clearBit (MV.length chunks - 1) 0) 0

part2 :: Text -> Int
part2 input = runST $ do
  chunks <- V.thaw $ parse input
  let go i j k
        | i < 0 = pure k
        | i < j = do
            (size, offset) <- MV.read chunks i
            go (i - 2) 1 $! k + i `div` 2 * triRange offset size
        | otherwise = do
            (size, _) <- MV.read chunks i
            (freeSize, freeOffset) <- MV.read chunks j
            if size <= freeSize
              then do
                MV.write chunks j (freeSize - size, freeOffset + size)
                go (i - 2) 1 $! k + i `div` 2 * triRange freeOffset size
              else go i (j + 2) k
  go (clearBit (MV.length chunks - 1) 0) 1 0
