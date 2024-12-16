{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module:         Day14
-- Description:    <https://adventofcode.com/2024/day/14 Day 14: Restroom Redoubt>
module Day14 (part1, part1', part2) where

import Common (groupConsecutiveBy)
import Control.Monad (join, liftM2)
import Control.Parallel.Strategies (parList, rdeepseq, withStrategy)
import Data.Char (intToDigit)
import Data.Map qualified as Map (findWithDefault)
import Data.Map.Strict qualified as Map (fromListWith)
import Data.Ord (Down (Down))
import Data.Set qualified as Set (fromList, toList)
import Data.String (IsString)
import Data.Text (Text)
import Data.Void (Void)
import Debug.Trace (traceM)
import Text.Megaparsec (MonadParsec, ParseErrorBundle, Stream (Token, Tokens), parse, sepEndBy1)
import Text.Megaparsec.Char (char, newline, string)
import Text.Megaparsec.Char.Lexer qualified as L (decimal, signed)

parser :: (MonadParsec e s m, IsString (Tokens s), Token s ~ Char, Num a) => m [((a, a), (a, a))]
parser = line `sepEndBy1` newline
  where
    line = (,) <$> (string "p=" *> v2) <*> (string " v=" *> v2)
    v2 = (,) <$> (L.signed (pure ()) L.decimal <* char ',') <*> L.signed (pure ()) L.decimal

part1 :: Text -> Either (ParseErrorBundle Text Void) Int
part1 = part1' 101 103

part1' :: Int -> Int -> Text -> Either (ParseErrorBundle Text Void) Int
part1' width height input = do
  robots <- parse parser "" input
  let totals =
        Map.fromListWith (+) $
          [ ((compare x $ width `div` 2, compare y $ height `div` 2), 1)
          | ((x0, y0), (vx, vy)) <- robots,
            let x = (x0 + vx * t) `mod` width
                y = (y0 + vy * t) `mod` height
          ]
  pure $ product [Map.findWithDefault 0 k totals | k <- join (liftM2 (,)) [LT, GT]]
  where
    t = 100

part2 :: Text -> Either (ParseErrorBundle Text Void) Int
part2 input = do
  robots <- parse parser "" input
  let (_, bestTime) =
        minimum . withStrategy (parList rdeepseq) $
          [ (Down $ maximum $ map length verticalLines, t)
          | t <- [0 .. lcm width height - 1],
            let verticalLines =
                  groupConsecutiveBy isLine . Set.toList . Set.fromList $
                    [ ((y0 + vy * t) `mod` height, (x0 + vx * t) `mod` width)
                    | ((x0, y0), (vx, vy)) <- robots
                    ]
                isLine (y0, x0) (y1, x1) = y0 == y1 && x0 + 1 == x1
          ]
      positions =
        Map.fromListWith (+) $
          [ (((x0 + vx * bestTime) `mod` width, (y0 + vy * bestTime) `mod` height), 1)
          | ((x0, y0), (vx, vy)) <- robots
          ]
      line y =
        [ case Map.findWithDefault 0 (x, y) positions of
            0 -> '.'
            n -> if n < 10 then intToDigit n else '+'
        | x <- [0 .. width - 1]
        ]
  mapM_ (traceM . line) [0 .. height - 1]
  pure bestTime
  where
    width = 101
    height = 103
