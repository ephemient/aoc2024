{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module:         Day17
-- Description:    <https://adventofcode.com/2024/day/17 Day 17: Chronospatial Computer>
module Day17 (part1, part2) where

import Control.Exception (assert)
import Data.Bits (Bits, shiftR, xor, (.&.))
import Data.Char (digitToInt)
import Data.Functor (void)
import Data.List (isSuffixOf)
import Data.String (IsString)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec, ParseErrorBundle, Stream (Token, Tokens), between, oneOf, parse, sepBy)
import Text.Megaparsec.Char (char, newline, string)
import Text.Megaparsec.Char.Lexer qualified as L (decimal)

parser :: (MonadParsec e s m, IsString (Tokens s), Token s ~ Char, Num a) => m ((a, a, a), [Int])
parser = do
  a <- between (string "Register A: ") newline L.decimal
  b <- between (string "Register B: ") newline L.decimal
  c <- between (string "Register C: ") newline L.decimal
  void newline
  program <- between (string "Program: ") newline $ (digitToInt <$> oneOf ['0' .. '7']) `sepBy` char ','
  pure ((a, b, c), program)

run :: (Bits a, Integral a) => [Int] -> (a, a, a) -> [a]
run program = run' 0
  where
    run' ip registers@(a, b, c)
      | ip < 0 || ip >= length program = []
      | 0 <- instruction = run' ip' (a `shiftR` fromIntegral (combo registers operand), b, c)
      | 1 <- instruction = run' ip' (a, b `xor` fromIntegral operand, c)
      | 2 <- instruction = run' ip' (a, combo registers operand .&. 7, c)
      | 3 <- instruction = run' (if a == 0 then ip + 2 else operand) registers
      | 4 <- instruction = run' ip' (a, b `xor` c, c)
      | 5 <- instruction = combo registers operand .&. 7 : run' ip' registers
      | 6 <- instruction = run' ip' (a, a `shiftR` fromIntegral (combo registers operand), b)
      | 7 <- instruction = run' ip' (a, b, a `shiftR` fromIntegral (combo registers operand))
      | otherwise = assert False []
      where
        instruction = program !! ip
        operand = program !! (ip + 1)
        ip' = ip + 2
    combo (a, _, _) 4 = a
    combo (_, b, _) 5 = b
    combo (_, _, c) 6 = c
    combo _ operand = fromIntegral operand

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
