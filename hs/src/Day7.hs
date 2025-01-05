{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module:         Day7
-- Description:    <https://adventofcode.com/2024/day/7 Day 7: Bridge Repair>
module Day7 (part1, part2) where

import Common (readEntire, readSome)
import Control.Monad (ap)
import Control.Parallel.Strategies (parMap, rseq)
import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty)
import Data.List.NonEmpty qualified as NE (reverse)
import Data.Text (Text)
import Data.Text qualified as T (lines, stripPrefix)
import Data.Text.Read (Reader)
import Data.Text.Read qualified as T (decimal)

parseLine :: (Integral a) => Reader (a, NonEmpty a)
parseLine line = do
  (lhs, line') <- T.decimal line
  (rhs, line'') <- maybe (Left "") (readSome T.decimal) $ T.stripPrefix ": " line'
  pure ((lhs, NE.reverse rhs), line'')

parFilter :: (a -> Bool) -> [a] -> [a]
parFilter f = fmap fst . filter snd . (zip `ap` parMap rseq f)

solve :: (Integral a) => (a -> a -> [a]) -> Text -> Either String a
solve op input = sum . map fst . parFilter f <$> mapM (readEntire parseLine) (T.lines input)
  where
    f (x, y :| rest)
      | Just rest' <- nonEmpty rest = or [f (z, rest') | z <- x `op` y]
      | otherwise = x == y

op1, op2 :: (Integral a) => a -> a -> [a]
op1 x y = [x - y | x >= y] <> case x `divMod` y of (z, 0) -> [z]; _ -> []
op2 x y =
  op1 x y
    <> [ z
       | d <- take 1 $ dropWhile (<= y) $ iterate (* 10) 10,
         let (z, r) = x `divMod` d,
         z /= 0 && r == y
       ]

part1, part2 :: Text -> Either String Int
part1 = solve op1
part2 = solve op2
