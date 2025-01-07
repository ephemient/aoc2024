{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

-- |
-- Module:         Day11
-- Description:    <https://adventofcode.com/2024/day/11 Day 11: Plutonian Pebbles>
module Day11 (part1, part2, solve) where

import Common (readMany)
import Control.Monad (foldM, forM_)
import Control.Monad.Primitive (PrimMonad, PrimState, primitive)
import Control.Monad.ST.Unsafe (unsafeInterleaveST)
import Control.Parallel.Strategies (parList, rseq, withStrategy)
import Data.Bifunctor (Bifunctor (bimap))
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap (empty, toList)
import Data.IntMap.Strict qualified as IntMap (insertWith)
import Data.Primitive.Array (Array, MutableArray (marray#), createArray, readArray, sizeofArray, sizeofMutableArray, writeArray)
import Data.Text (Text)
import Data.Text.Read qualified as T (decimal)
import GHC.Conc (getNumCapabilities)
import GHC.Exts (Int (I#), casArray#, readArray#, toList)

part1, part2 :: Text -> IO Int
part1 = solve 25
part2 = solve 75

solve :: Int -> Text -> IO Int
solve n input = do
  (nums, _) <- either fail pure $ readMany T.decimal input
  numCapabilities <- max 1 <$> getNumCapabilities
  let start = createArray numCapabilities IntMap.empty $ \array -> forM_ nums $ \num -> do
        let ix = num `mod` sizeofMutableArray array
        readArray array ix >>= writeArray array ix . IntMap.insertWith (+) num 1
      end = iterate step start !! n
  pure $ foldl' (foldl' (+)) 0 end

step :: Array (IntMap Int) -> Array (IntMap Int)
step array = createArray (sizeofArray array) IntMap.empty $ \array' -> do
  let insert x = modifyArray array' (x `mod` sizeofMutableArray array') . IntMap.insertWith (+) x
      go _ (0, n) = insert 1 n
      go _ (x, n)
        | s <- show x,
          (l, 0) <- length s `divMod` 2 = do
            let (y, z) = bimap read read $ splitAt l s
            insert y n
            insert z n
        | otherwise = insert (2024 * x) n
  results <- mapM (unsafeInterleaveST . foldM go () . IntMap.toList) $ toList array
  pure $! foldl' (flip seq) () $ withStrategy (parList rseq) results

modifyArray :: (PrimMonad m) => MutableArray (PrimState m) a -> Int -> (a -> a) -> m ()
modifyArray array (I# index#) f = primitive $ \s1# ->
  let array# = marray# array
      modifyArray# s3# b =
        case casArray# array# index# b (f b) s3# of
          (# s4#, 0#, _ #) -> (# s4#, () #)
          (# s4#, _, c #) -> modifyArray# s4# c
      !(# s2#, a #) = readArray# array# index# s1#
   in modifyArray# s2# a
