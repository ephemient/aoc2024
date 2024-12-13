{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module:         Day13
-- Description:    <https://adventofcode.com/2024/day/13 Day 13: Claw Contraption>
module Day13 (part1, part2) where

import Data.Maybe (mapMaybe)
import Data.String (IsString)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec, ParseErrorBundle, Stream (Token, Tokens), parse, sepEndBy)
import Text.Megaparsec.Char (newline, string)
import Text.Megaparsec.Char.Lexer qualified as L (decimal)

parser :: (MonadParsec e s m, IsString (Tokens s), Token s ~ Char, Num a) => m [(a, a, a, a, a, a)]
parser =
  flip sepEndBy newline $
    (,,,,,)
      <$> (string "Button A: X+" *> L.decimal)
      <*> (string ", Y+" *> L.decimal <* newline)
      <*> (string "Button B: X+" *> L.decimal)
      <*> (string ", Y+" *> L.decimal <* newline)
      <*> (string "Prize: X=" *> L.decimal)
      <*> (string ", Y=" *> L.decimal <* newline)

solve :: (Integral a) => (a, a, a, a, a, a) -> Maybe a
solve (ax, ay, bx, by_, x, y)
  | (a, 0) <- (x * by_ - y * bx) `divMod` (ax * by_ - bx * ay),
    (b, 0) <- (x * ay - y * ax) `divMod` (ay * bx - by_ * ax) =
      Just $ 3 * a + b
solve _ = Nothing

part1 :: Text -> Either (ParseErrorBundle Text Void) Int
part1 input = sum . mapMaybe solve <$> parse parser "" input

part2 :: Text -> Either (ParseErrorBundle Text Void) Int
part2 input = sum . mapMaybe solve' <$> parse parser "" input
  where
    solve' (ax, ay, bx, by_, x, y) = solve (ax, ay, bx, by_, x + 10000000000000, y + 10000000000000)
