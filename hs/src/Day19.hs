{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module:         Day19
-- Description:    <https://adventofcode.com/2024/day/19 Day 19: Linen Layout>
module Day19 (solve) where

import Control.Arrow ((***))
import Control.Monad.ST (runST)
import Data.Foldable (foldMap')
import Data.List (sort)
import Data.Monoid (Sum (Sum, getSum))
import Data.Text (Text)
import Data.Text qualified as T (isPrefixOf, length, lines, null, splitOn, tails)
import Data.Vector.Unboxed.Mutable qualified as MV (new, read, write)

count :: [Text] -> Text -> (Sum Int, Sum Int)
count keys target = runST $ do
  acc <- MV.new $ T.length target + 1
  MV.write acc 0 1
  mapM_ (\(i, j) -> (+) <$> MV.read acc i <*> MV.read acc j >>= MV.write acc i) . sort $
    [ (offset + T.length key, offset)
    | key <- keys,
      (offset, target') <- zip [0 ..] $ T.tails target,
      key `T.isPrefixOf` target'
    ]
  n <- MV.read acc $ T.length target
  pure (Sum $ if n == 0 then 0 else 1, Sum n)

solve :: Text -> (Int, Int)
solve input
  | keys : rest <- T.lines input =
      (getSum *** getSum) $ foldMap' (count $ T.splitOn ", " keys) $ filter (not . T.null) rest
  | otherwise = (0, 0)
