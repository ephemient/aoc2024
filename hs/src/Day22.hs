{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

-- |
-- Module:         Day22
-- Description:    <https://adventofcode.com/2024/day/22 Day 22: Monkey Market>
module Day22 (part1, part2) where

import Common (readMany, readSome)
import Control.Monad.Primitive (PrimMonad, PrimState, primitive)
import Control.Monad.ST (ST, runST)
import Control.Monad.ST.Unsafe (unsafeInterleaveST)
import Control.Parallel.Strategies (parTraversable, rseq, withStrategy)
import Data.Array.ST (MArray (newArray), STUArray, readArray, writeArray)
import Data.Bits (Bits, bit, shiftL, shiftR, testBit, xor, (.&.))
import Data.Foldable (Foldable (foldMap'))
import Data.Ix (index, rangeSize)
import Data.List (tails)
import Data.Maybe (fromJust)
import Data.Primitive (MutablePrimArray (MutablePrimArray), newPrimArray, setPrimArray)
import Data.Semigroup (Max (Max, getMax), sconcat)
import Data.Text (Text)
import Data.Text.Read qualified as T (decimal)
import Data.Vector.Unboxed qualified as V (generate, (!))
import GHC.Exts (Int (I#), fetchAddIntArray#)

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

part2 :: Text -> Either String Int
part2 input = do
  (nums, _) <- readSome T.decimal input
  pure $ runST $ do
    acc <- newPrimArray $ rangeSize bounds
    setPrimArray acc 0 (rangeSize bounds) 0
    let go num =
          fromJust <$> do
            seen <- newArray bounds False :: ST s (STUArray s _ _)
            let f (a : b : c : d : e : _) =
                  let key = (a - b, b - c, c - d, d - e)
                   in readArray seen key >>= \case
                        True -> pure Nothing
                        False -> do
                          writeArray seen key True
                          Just . Max . (+) e <$> fetchAddIntArray acc (index bounds key) e
                f _ = pure Nothing
            foldMap' f $ tails $ map (`mod` 10) $ take 2001 $ iterate step num
    getMax . sconcat . withStrategy (parTraversable rseq) <$> mapM (unsafeInterleaveST . go) nums
  where
    bounds = ((-9, -9, -9, -9), (9, 9, 9, 9))

fetchAddIntArray :: (PrimMonad m) => MutablePrimArray (PrimState m) Int -> Int -> Int -> m Int
fetchAddIntArray (MutablePrimArray mba#) (I# offset#) (I# incr#) = primitive $ \s1# ->
  let !(# s2#, res# #) = fetchAddIntArray# mba# offset# incr# s1#
   in (# s2#, I# res# #)
