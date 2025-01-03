-- |
-- Module:         Day22
-- Description:    <https://adventofcode.com/2024/day/22 Day 22: Monkey Market>
module Day22 (part1, part2) where

import Common (readMany, readSome)
import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.STM (TArray, atomically)
import Data.Array.IO (IOUArray)
import Data.Array.MArray (newArray, readArray, writeArray)
import Data.Bits (Bits, bit, shiftL, shiftR, testBit, xor, (.&.))
import Data.Foldable (Foldable (foldMap'))
import Data.Functor (($>))
import Data.List (tails)
import Data.Semigroup (Max (Max, getMax), sconcat)
import Data.Text (Text)
import Data.Text.Read qualified as T (decimal)
import Data.Vector.Unboxed qualified as V (generate, (!))

step :: (Bits a, Num a) => a -> a
step num = num3
  where
    num1 = (num `xor` num `shiftL` 6) .&. 16777215
    num2 = (num1 `xor` num1 `shiftR` 5) .&. 16777215
    num3 = (num2 `xor` num2 `shiftL` 11) .&. 16777215

part1 :: Text -> Either String Int
part1 input = do
  (nums, _) <- readMany @Int T.decimal input
  pure $ sum [foldl' xor 0 [constants V.! i | i <- [0 .. 23], testBit num i] | num <- nums]
  where
    constants = V.generate 24 $ (!! 2000) . iterate step . bit

part2 :: Text -> IO Int
part2 input = case readSome T.decimal input of
  Right (nums, _) -> do
    acc <- newArray @TArray bounds 0
    let go num = do
          seen <- newArray @IOUArray bounds False
          let f (a : b : c : d : e : _) =
                let key = (a - b, b - c, c - d, d - e)
                 in readArray seen key >>= \case
                      True -> pure Nothing
                      False -> do
                        writeArray seen key True
                        atomically $ do
                          total <- (+) e <$> readArray acc key
                          writeArray acc key total $> Just (Max total)
              f _ = pure Nothing
          foldMap' f (tails $ take 2001 $ map (`mod` 10) $ iterate step num)
    mapConcurrently go nums >>= maybe (fail "error") (pure . getMax) . sconcat
  Left err -> fail err
  where
    bounds = ((-9, -9, -9, -9), (9, 9, 9, 9))
