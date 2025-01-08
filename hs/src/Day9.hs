-- |
-- Module:         Day9
-- Description:    <https://adventofcode.com/2024/day/9 Day 9: Disk Defragmenter>
module Day9 (part1, part2) where

import Control.Monad (ap, foldM, when)
import Control.Monad.ST (runST)
import Data.Char (digitToInt, isDigit)
import Data.List (scanl')
import Data.List.Split (chunksOf)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T (unpack)
import Data.Vector (Vector)
import Data.Vector qualified as V (fromList, thaw)
import Data.Vector.Mutable qualified as MV (length, read, replicate, write)

parse :: Text -> Vector (Int, (Int, Int))
parse input =
  V.fromList $
    flip zip `ap` scanl' (flip $ (+) . uncurry (+)) 0 $
      [ (size, fromMaybe 0 $ listToMaybe free)
      | size : free <- chunksOf 2 $ map digitToInt $ filter isDigit $ T.unpack input
      ]

triRange :: (Integral a) => a -> a -> a
triRange offset size = (2 * offset + size - 1) * size `div` 2

part1 :: Text -> Int
part1 input = runST $ do
  files <- V.thaw $ parse input
  let go i j k
        | i < j = do
            (offset, (used, free)) <- MV.read files i
            go' (offset + used) free i j $! k + i * triRange offset used
        | otherwise = pure k
      go' offset free i j k
        | free > 0 && i + 1 < j = do
            (offset', (used', free')) <- MV.read files $ j - 1
            let moved = min free used'
            if moved < used'
              then do
                MV.write files (j - 1) (offset', (used' - moved, free' + moved))
                go (i + 1) j $! k + (j - 1) * triRange offset moved
              else go' (offset + moved) (free - moved) i (j - 1) $! k + (j - 1) * triRange offset moved
        | otherwise = go (i + 1) j k
  go 0 (MV.length files) 0

part2 :: Text -> Int
part2 input = runST $ do
  files <- V.thaw $ parse input
  starts <- MV.replicate 10 0
  let go k i = do
        (offset, (used, _)) <- MV.read files i
        (j, offset') <- fromMaybe (i, offset) <$> (MV.read starts used >>= go' used i)
        go'' used j
        pure $! k + i * triRange offset' used
      go' used i j
        | j < i = do
            (offset, (used', free)) <- MV.read files j
            if used <= free
              then do
                MV.write files j (offset, (used', free - used))
                Just . (j,) . subtract free . fst <$> MV.read files (j + 1)
              else go' used i $ j + 1
        | otherwise = pure Nothing
      go'' used i
        | used < MV.length starts = do
            j <- MV.read starts used
            when (j < i) $ do
              MV.write starts used i
              go'' (used + 1) i
        | otherwise = pure ()
  foldM go 0 [MV.length files - 1, MV.length files - 2 .. 0]
