{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

-- |
-- Module:         Day11
-- Description:    <https://adventofcode.com/2024/day/11 Day 11: Plutonian Pebbles>
module Day11 (part1, part2, solve) where

import Common (readMany)
import Control.Monad (forM_)
import Control.Monad.Primitive (PrimMonad, PrimState, primitive)
import Control.Monad.ST.Unsafe (unsafeInterleaveST)
import Control.Parallel.Strategies (parTraversable, rseq, withStrategy)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap (empty, toList)
import Data.IntMap.Strict qualified as IntMap (insertWith)
import Data.Primitive.Array (Array, MutableArray (marray#), createArray, sizeofArray, sizeofMutableArray)
import Data.Text (Text)
import Data.Text.Read qualified as T (decimal)
import GHC.Conc (getNumCapabilities)
import GHC.Exts (Int (I#), casArray#, readArray#)

part1, part2 :: Text -> IO Int
part1 = solve 25
part2 = solve 75

solve :: Int -> Text -> IO Int
solve n input = do
  (nums, _) <- either fail pure $ readMany T.decimal input
  numCapabilities <- max 1 <$> getNumCapabilities
  let start = createArray numCapabilities IntMap.empty $ \array ->
        forM_ nums $ \x -> insert array x 1
      end = iterate step start !! n
  pure $ foldl' (foldl' (+)) 0 end

step :: Array (IntMap Int) -> Array (IntMap Int)
step array = createArray (sizeofArray array) IntMap.empty $ \array' -> do
  let go (0, n) = insert array' 1 n
      go (x, n)
        | Just (y, z) <- splitDigits x = insert array' y n >> insert array' z n
        | otherwise = insert array' (2024 * x) n
  results <- mapM (unsafeInterleaveST . mapM_ go . IntMap.toList) array
  pure $! foldl' (flip seq) () $ withStrategy (parTraversable rseq) results

splitDigits :: (Integral a) => a -> Maybe (a, a)
splitDigits x = splitDigits' x 10
  where
    splitDigits' y n
      | y < 10 = Nothing
      | y < 100 = Just $ x `divMod` n
      | otherwise = splitDigits' (y `div` 100) $! 10 * n

insert :: (PrimMonad m, Num a) => MutableArray (PrimState m) (IntMap a) -> Int -> a -> m (IntMap a)
insert array x = modifyArray array (x `mod` sizeofMutableArray array) . IntMap.insertWith (+) x

modifyArray :: (PrimMonad m) => MutableArray (PrimState m) a -> Int -> (a -> a) -> m a
modifyArray array (I# index#) f = primitive $ \s1# ->
  let !(# s2#, a #) = readArray# array# index# s1#
   in modifyArray# s2# a
  where
    array# = marray# array
    modifyArray# s1# a =
      case casArray# array# index# a (f a) s1# of
        (# s2#, 0#, b #) -> (# s2#, b #)
        (# s2#, _, b #) -> modifyArray# s2# b
