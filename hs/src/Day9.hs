{-# LANGUAGE ParallelListComp #-}

-- |
-- Module:         Day9
-- Description:    <https://adventofcode.com/2024/day/9 Day 9: Disk Defragmenter>
module Day9 (part1, part2) where

import Control.Monad (ap)
import Data.Char (digitToInt, isDigit)
import Data.List (mapAccumR, scanl')
import Data.List.Split (chunksOf)
import Data.Sequence qualified as Seq (Seq ((:<|), (:|>)), fromList, length, spanl, take, (<|), (|>))
import Data.Text (Text)
import Data.Text qualified as T (unpack)

triRange :: (Integral a) => a -> a -> a
triRange offset size = (2 * offset + size - 1) * size `div` 2

part1 :: Text -> Int
part1 input = snd $ foldl' add (0, 0) disk1
  where
    disk0 = zip ([0 ..] >>= (: [Nothing]) . Just) . map digitToInt . filter isDigit $ T.unpack input
    disk1 = defrag . Seq.fromList $ disk0
    defrag (disk Seq.:|> (Nothing, _)) = defrag disk
    defrag ((Just name, size) Seq.:<| disk) = (name, size) : defrag disk
    defrag ((Nothing, freeSize) Seq.:<| (disk Seq.:|> (Just name, size)))
      | size <= freeSize = (name, size) : defrag ((Nothing, freeSize - size) Seq.<| disk)
      | otherwise = (name, freeSize) : defrag (disk Seq.|> (Just name, size - freeSize))
    defrag _ = []
    add (offset, !total) (name, size) = (offset + size, total + name * triRange offset size)

part2 :: Text -> Int
part2 input =
  sum
    [ name * triRange offset size
    | (name, offset, size) <- snd $ mapAccumR defrag free0 disk0
    ]
  where
    disk0 =
      [ (name, offset, size)
      | name <- [0 ..]
      | (size, offset) : _ <- chunksOf 2 . (zip `ap` scanl' (+) 0) . map digitToInt $ T.unpack input
      ]
    free0 =
      Seq.fromList
        [ (start, end - start)
        | (_, offset, size) <- disk0,
          let start = offset + size
        | (_, end, _) <- drop 1 disk0
        ]
    defrag free (name, _, size)
      | (free1, (freeOffset, freeSize) Seq.:<| free2) <- Seq.spanl (\(_, freeSize) -> freeSize < size) free =
          ( free1 <> Seq.take (Seq.length free2) ((freeOffset + size, freeSize - size) Seq.<| free2),
            (name, freeOffset, size)
          )
    defrag free file = (Seq.take (Seq.length free - 1) free, file)
