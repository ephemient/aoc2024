{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module:         Day17
-- Description:    <https://adventofcode.com/2024/day/17 Day 17: Chronospatial Computer>
module Day17 (part1, part2, run, step) where

import Data.Bits (shiftR, xor, (.&.))
import Data.List (isSuffixOf, unfoldr)
import Data.Maybe (catMaybes)
import Data.String (IsString)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec, ParseErrorBundle, Stream (Token, Tokens), between, parse, sepBy)
import Text.Megaparsec.Char (char, newline, string)
import Text.Megaparsec.Char.Lexer qualified as L (decimal)

parser :: (MonadParsec e s m, IsString (Tokens s), Token s ~ Char, Num a) => m ((a, a, a), [a])
parser = do
  a <- between (string "Register A: ") newline L.decimal
  b <- between (string "Register B: ") newline L.decimal
  c <- between (string "Register C: ") newline L.decimal
  newline
  program <- between (string "Program: ") newline $ L.decimal `sepBy` char ','
  pure ((a, b, c), program)

step :: [Int] -> (Int, (Int, Int, Int)) -> Maybe (Maybe Int, (Int, (Int, Int, Int)))
step program (ip, registers@(a, b, c))
  | ip < 0 || ip >= length program = Nothing
  | 0 <- instruction = Just (Nothing, (ip + 2, (a `shiftR` combo, b, c)))
  | 1 <- instruction = Just (Nothing, (ip + 2, (a, b `xor` operand, c)))
  | 2 <- instruction = Just (Nothing, (ip + 2, (a, combo .&. 7, c)))
  | 3 <- instruction = Just (Nothing, (if a == 0 then ip + 2 else operand, registers))
  | 4 <- instruction = Just (Nothing, (ip + 2, (a, b `xor` c, c)))
  | 5 <- instruction = Just (Just $ combo .&. 7, (ip + 2, registers))
  | 6 <- instruction = Just (Nothing, (ip + 2, (a, a `shiftR` combo, c)))
  | 7 <- instruction = Just (Nothing, (ip + 2, (a, b, a `shiftR` combo)))
  where
    instruction = program !! ip
    operand = program !! (ip + 1)
    combo
      | 0 <= operand && operand <= 3 = operand
      | 4 <- operand = a
      | 5 <- operand = b
      | 6 <- operand = c

run :: [Int] -> (Int, Int, Int) -> [Int]
run program = catMaybes . unfoldr (step program) . (0,)

part1 :: Text -> Either (ParseErrorBundle Text Void) [Int]
part1 input = do
  (registers, program) <- parse parser "" input
  pure $ run program registers

part2 :: Text -> Either (ParseErrorBundle Text Void) Int
part2 input = do
  ((_, b, c), program) <- parse parser "" input
  let go nums
        | (a, _) : _ <- filter ((== program) . snd) next = a
        | otherwise = go $ fst <$> next
        where
          next =
            [ (a, output)
            | a <- (+) . (8 *) <$> nums <*> [0 .. 7],
              let output = run program (a, b, c),
              output `isSuffixOf` program
            ]
  pure $ go [0]
