-- |
-- Module:         Day22
-- Description:    <https://adventofcode.com/2024/day/22 Day 22: Monkey Market>
module Day22 (part1, part2) where

import Common (readMany)
import Control.Monad.ST (ST, runST)
import Control.Monad.ST.Unsafe (unsafeInterleaveST)
import Control.Parallel.Strategies (parList, rseq, withStrategy)
import Data.Array.ST (Ix, MArray (newArray), STUArray, modifyArray', readArray, writeArray)
import Data.Bits (bit, shiftL, shiftR, testBit, xor, (.&.))
import Data.Foldable (Foldable (foldMap'))
import Data.List (tails)
import Data.Semigroup (Max (Max, getMax))
import Data.Text (Text)
import Data.Text.Read qualified as T (decimal)
import Data.Vector.Unboxed qualified as V (generate, (!))

step :: Int -> Int
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

part2 :: Text -> Either String Int
part2 input = do
  (nums, _) <- readMany T.decimal input
  pure $ runST $ do
    acc <- newSTUArray ((-9, -9, -9, -9), (9, 9, 9, 9)) 0
    let f num = do
          seen <- newSTUArray ((-9, -9, -9, -9), (9, 9, 9, 9)) False
          let prices = map (`mod` 10) $ take 2001 $ iterate step num
              g (key, price) =
                readArray seen key >>= \case
                  True -> pure Nothing
                  False -> do
                    writeArray seen key True
                    modifyArray' acc key (+ price)
                    Just . Max <$> readArray acc key
          foldMap' g [((a - b, b - c, c - d, d - e), e) | a : b : c : d : e : _ <- tails prices]
    maybe 0 getMax . mconcat . withStrategy (parList rseq) <$> mapM (unsafeInterleaveST . f) nums
  where
    newSTUArray :: forall s i e. (Ix i, MArray (STUArray s) e (ST s)) => (i, i) -> e -> ST s (STUArray s i e)
    newSTUArray = newArray
