{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module:         Day7
-- Description:    <https://adventofcode.com/2024/day/7 Day 7: Bridge Repair>
module Day7 (part1, part2) where

import Common (readEntire, readSome)
import Control.Monad (ap)
import Control.Parallel.Strategies (parMap, rseq)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text)
import Data.Text qualified as T (lines, stripPrefix)
import Data.Text.Read (Reader)
import Data.Text.Read qualified as T (decimal)

parseLine :: (Integral a) => Reader (a, NonEmpty a)
parseLine line = do
  (lhs, line') <- T.decimal line
  (rhs, line'') <- maybe (Left "") (readSome T.decimal) $ T.stripPrefix ": " line'
  pure ((lhs, rhs), line'')

parFilter :: (a -> Bool) -> [a] -> [a]
parFilter f = fmap fst . filter snd . (zip `ap` parMap rseq f)

solve :: (Integral a) => (a -> a -> [a]) -> Text -> Either String a
solve op input = sum . map fst . parFilter f <$> mapM (readEntire parseLine) (T.lines input)
  where
    f (lhs, r0 :| []) = lhs == r0
    f (lhs, r0 :| r1 : rhs) = or [r2 <= lhs && f (lhs, r2 :| rhs) | r2 <- r0 `op` r1]

part1 :: Text -> Either String Int
part1 = solve $ \a b -> [a + b, a * b]

part2 :: Text -> Either String Int
part2 = solve $ \a b -> [a + b, a * b, read $ show a ++ show b]
