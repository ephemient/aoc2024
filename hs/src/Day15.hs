{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module:         Day15
-- Description:    <https://adventofcode.com/2024/day/15 Day 15: Warehouse Woes>
module Day15 (part1, part2) where

import Control.Exception (assert)
import Data.Array.Unboxed (IArray, UArray, assocs, listArray, (!), (//))
import Data.Bifunctor (first, second)
import Data.List (sort)
import Data.Text (Text)
import Data.Text qualified as T (breakOn, concat, concatMap, foldl', justifyLeft, length, lines, singleton, unpack)
import Data.Text.Read (Reader)

parse :: (IArray a Char) => Reader (a (Int, Int) Char)
parse input
  | null grid = Left "empty warehouse"
  | otherwise =
      Right
        ( listArray ((0, 0), (height - 1, width - 1)) . T.unpack . T.concat $
            T.justifyLeft width '.' <$> grid,
          rest
        )
  where
    (grid, rest) = first T.lines $ T.breakOn "\n\n" input
    height = length grid
    width = maximum $ 0 : map T.length grid

part1 :: Text -> Either String Int
part1 input = do
  (grid0, moves) <- parse @UArray input
  pos0 <- case filter ((== '@') . snd) $ assocs grid0 of
    [(pos0, _)] -> Right pos0
    _ -> Left "can't find @"
  pure . sum $
    [ 100 * y + x
    | ((y, x), c) <- assocs . fst $ T.foldl' move (grid0, pos0) moves,
      c == 'O' || c == '['
    ]
  where
    move state@(grid, pos) c
      | '^' <- c = move' moveY (first pred pos) (-1)
      | 'v' <- c = move' moveY (first succ pos) 1
      | '<' <- c = move' moveX (second pred pos) (-1)
      | '>' <- c = move' moveX (second succ pos) 1
      where
        move' mover pos' delta =
          assert (grid ! pos == '@') . maybe state ((,pos') . (grid //) . sort) $
            mover delta pos' [(pos', '@'), (pos, '.')]
        moveY dy pos'@(y, x) k = case grid ! pos' of
          '.' -> Just k
          'O' -> moveY dy (y + dy, x) $ ((y + dy, x), 'O') : (pos', '.') : k
          '[' -> assert (grid ! (y, x + 1) == ']') $ do
            k' <-
              moveY dy (y + dy, x) $
                ((y + dy, x), '[') : ((y + dy, x + 1), ']') : ((y, x), '.') : ((y, x + 1), '.') : k
            moveY dy (y + dy, x + 1) k'
          ']' -> assert (grid ! (y, x - 1) == '[') $ do
            k' <-
              moveY dy (y + dy, x) $
                ((y + dy, x - 1), '[') : ((y + dy, x), ']') : ((y, x - 1), '.') : ((y, x), '.') : k
            moveY dy (y + dy, x - 1) k'
          _ -> Nothing
        moveX dx pos'@(y, x) k = case grid ! pos' of
          '.' -> Just k
          '#' -> Nothing
          d -> moveX dx (y, x + dx) $ ((y, x + dx), d) : (pos', '.') : k
    move state _ = state

part2 :: Text -> Either String Int
part2 = part1 . T.concatMap f
  where
    f '#' = "##"
    f '.' = ".."
    f '@' = "@."
    f 'O' = "[]"
    f c = T.singleton c
