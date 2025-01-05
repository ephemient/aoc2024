{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module:         Day18
-- Description:    <https://adventofcode.com/2024/day/18 Day 18: RAM Run>
module Day18 (part1, part1', part2, part2') where

import Common (readEntire)
import Control.Monad (ap, join, liftM2)
import Control.Monad.Loops (firstM)
import Control.Monad.ST (runST)
import Data.Function (on)
import Data.Functor (($>))
import Data.IntSet qualified as IntSet (empty, fromList, insert, notMember)
import Data.List (scanl')
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T (lines, stripPrefix)
import Data.Text.Read (Reader)
import Data.Text.Read qualified as T (decimal)
import Data.Vector.Unboxed.Mutable qualified as MV (generate, length, read, write)

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
  maybe (Left "no solution") Right $ go (IntSet.fromList $ 0 : map index coords) [((0, 0), 0)] []
  where
    index (x, y) = x * (size + 1) + y
    go visited (((x, y), t) : queue) queue'
      | x == size && y == size = Just t
      | otherwise = go (foldl' (flip $ IntSet.insert . index) visited next) queue $ map (,t + 1) next ++ queue'
      where
        next =
          [ pos'
          | pos'@(x', y') <- [(x - 1, y), (x, y - 1), (x, y + 1), (x + 1, y)],
            0 <= x' && x' <= size && 0 <= y' && y' <= size && index pos' `IntSet.notMember` visited
          ]
    go _ _ [] = Nothing
    go visited [] queue = go visited (reverse queue) []

part2 :: Text -> Either String (Int, Int)
part2 = part2' 70

part2' :: Int -> Text -> Either String (Int, Int)
part2' size input = do
  candidates <-
    reverse
      . filter (uncurry $ IntSet.notMember . index)
      . (zip `ap` scanl' (flip $ IntSet.insert . index) IntSet.empty)
      <$> mapM (readEntire coord) (T.lines input)
  let obstacles0 = maybe IntSet.empty (uncurry $ IntSet.insert . index) $ listToMaybe candidates
  maybe (Left "No solution") (Right . fst) $ runST $ do
    acc <- MV.generate (join (*) $ size + 1) id
    let root key = MV.read acc key >>= root' key
        root' key value
          | key == value = pure value
          | otherwise = do
              value' <- root value
              MV.write acc key value' $> value'
        union i j = join $ MV.write acc <$> root i <*> root j
    sequence_
      [ (union `on` index) pos pos'
      | pos@(x, y) <- join (liftM2 (,)) [0 .. size],
        index pos `IntSet.notMember` obstacles0,
        pos' <- [(x, y + 1) | y < size] ++ [(x + 1, y) | x < size],
        index pos' `IntSet.notMember` obstacles0
      ]
    flip firstM candidates $ \(pos@(x, y), obstacles) -> do
      sequence_
        [ (union `on` index) pos pos'
        | pos' <- [(x - 1, y) | x > 0] ++ [(x, y - 1) | y > 0] ++ [(x, y + 1) | y < size] ++ [(x + 1, y) | x < size],
          index pos' `IntSet.notMember` obstacles
        ]
      (==) <$> root 0 <*> root (MV.length acc - 1)
  where
    index (x, y) = x * (size + 1) + y
