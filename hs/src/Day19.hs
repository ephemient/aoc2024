{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module:         Day19
-- Description:    <https://adventofcode.com/2024/day/19 Day 19: Linen Layout>
module Day19 (solve) where

import Control.Arrow ((***))
import Control.Monad (forM_, when)
import Control.Monad.ST (runST)
import Data.Foldable (foldMap')
import Data.Monoid (Sum (Sum, getSum))
import Data.Text (Text)
import Data.Text qualified as T (isPrefixOf, length, lines, null, splitOn, tails)
import Data.Vector.Unboxed.Mutable qualified as MV (modify, new, read, write)

count :: [Text] -> Text -> (Sum Int, Sum Int)
count keys target = runST $ do
  acc <- MV.new $ T.length target + 1
  MV.write acc 0 1
  forM_ (zip [0 ..] $ T.tails target) $ \(i, target') -> do
    n <- MV.read acc i
    forM_ keys $ \key ->
      when (key `T.isPrefixOf` target') $ MV.modify acc (+ n) $ i + T.length key
  n <- MV.read acc $ T.length target
  pure (Sum $ if n == 0 then 0 else 1, Sum n)

solve :: Text -> (Int, Int)
solve input
  | keys : rest <- T.lines input =
      (getSum *** getSum) $ foldMap' (count $ T.splitOn ", " keys) $ filter (not . T.null) rest
  | otherwise = (0, 0)
