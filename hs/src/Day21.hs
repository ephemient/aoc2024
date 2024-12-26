-- |
-- Module:         Day21
-- Description:    <https://adventofcode.com/2024/day/21 Day 21: Keypad Conundrum>
module Day21 (solve) where

import Control.Exception (assert)
import Data.Array.Unboxed (UArray, listArray, range, (!))
import Data.Char (digitToInt, isDigit)
import Data.Function (on)
import Data.List (permutations)
import Data.Text (Text)
import Data.Text qualified as T (foldl', lines, unpack)

luts :: [UArray ((Int, Int), (Int, Int)) Int]
luts =
  iterate buildLut . listArray arrayBounds $
    [abs (x2 - x1) + abs (y2 - y1) + 1 | ((x1, y1), (x2, y2)) <- range arrayBounds]
  where
    (minXY, maxXY) = ((0, -1), (2, 3))
    arrayBounds = ((minXY, minXY), (maxXY, maxXY))
    buildLut lut =
      listArray arrayBounds $
        [ foldl' min maxBound $ map cost $ filter check $ permutations moves
        | ((x1, y1), (x2, y2)) <- range arrayBounds,
          let moves =
                replicate (x2 - x1) (2, -1)
                  ++ replicate (y2 - y1) (1, 0)
                  ++ replicate (y1 - y2) (1, -1)
                  ++ replicate (x1 - x2) (0, -1)
              check = notElem (0, 0) . scanl move (x1, y1)
              cost string = sum $ zipWith (curry (lut !)) ((2, 0) : string) (string ++ [(2, 0)])
        ]
    move (x, y) (1, 0) = (x, y + 1)
    move (x, y) (0, -1) = (x - 1, y)
    move (x, y) (1, -1) = (x, y - 1)
    move (x, y) (2, -1) = (x + 1, y)
    move pos _ = assert False pos

solve :: Int -> Text -> Int
solve depth input = sum [cost (T.unpack line) * T.foldl' accumDigits 0 line | line <- T.lines input]
  where
    cost string = sum $ zipWith (curry (luts !! depth !) `on` pos) ('A' : string) string
    accumDigits n d | isDigit d = 10 * n + digitToInt d
    accumDigits n _ = n
    pos '0' = (1, 0)
    pos '1' = (0, 1)
    pos '2' = (1, 1)
    pos '3' = (2, 1)
    pos '4' = (0, 2)
    pos '5' = (1, 2)
    pos '6' = (2, 2)
    pos '7' = (0, 3)
    pos '8' = (1, 3)
    pos '9' = (2, 3)
    pos _ = (2, 0)
