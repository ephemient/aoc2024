{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module:         Day14
-- Description:    <https://adventofcode.com/2024/day/14 Day 14: Restroom Redoubt>
module Day14 (part1, part1', part2) where

import Common (crt)
import Control.Monad (ap)
import Data.Foldable (foldMap')
import Data.Semigroup (Arg (Arg), Sum (Sum))
import Data.String (IsString)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec, ParseErrorBundle, Stream (Token, Tokens), parse, sepEndBy1)
import Text.Megaparsec.Char (char, newline, string)
import Text.Megaparsec.Char.Lexer qualified as L (decimal, signed)

parser :: (MonadParsec e s m, IsString (Tokens s), Token s ~ Char, Num a) => m [((a, a), (a, a))]
parser = line `sepEndBy1` newline
  where
    line = do
      x <- string "p=" *> L.signed (pure ()) L.decimal
      y <- char ',' *> L.signed (pure ()) L.decimal
      vx <- string " v=" *> L.signed (pure ()) L.decimal
      vy <- char ',' *> L.signed (pure ()) L.decimal
      pure ((x, vx), (y, vy))

part1 :: Text -> Either (ParseErrorBundle Text Void) Int
part1 = part1' 101 103

part1' :: Int -> Int -> Text -> Either (ParseErrorBundle Text Void) Int
part1' width height input = do
  robots <- parse parser "" input
  let (Sum q1, Sum q2, Sum q3, Sum q4) = foldMap' f robots
  pure $ q1 * q2 * q3 * q4
  where
    t = 100
    f ((x, vx), (y, vy)) = case ( compare ((x + t * vx) `mod` width) (width `div` 2),
                                  compare ((y + t * vy) `mod` height) (height `div` 2)
                                ) of
      (LT, LT) -> (Sum 1, Sum 0, Sum 0, Sum 0)
      (LT, GT) -> (Sum 0, Sum 1, Sum 0, Sum 0)
      (GT, LT) -> (Sum 0, Sum 0, Sum 1, Sum 0)
      (GT, GT) -> (Sum 0, Sum 0, Sum 0, Sum 1)
      _ -> mempty

part2 :: Text -> Either (ParseErrorBundle Text Void) Int
part2 input = do
  (xrobots, yrobots) <- unzip <$> parse parser "" input
  let Arg _ x = maximum $ (flip Arg `ap` score xrobots width) <$> [0 .. width - 1]
      Arg _ y = maximum $ (flip Arg `ap` score yrobots height) <$> [0 .. height - 1]
  pure $ fst $ crt (x, width) (y, height)
  where
    (width, height) = (101, 103)
    score robots m t = max h1 h2 :: Int
      where
        (Sum h1, Sum h2) = foldMap' f robots
        f (p, v) = case compare ((p + t * v) `mod` m) (m `div` 2) of
          LT -> (Sum 1, Sum 0)
          EQ -> (Sum 0, Sum 0)
          GT -> (Sum 0, Sum 1)
