module Common (groupConsecutiveBy, readEntire, readMany, readSome) where

import Control.Arrow (first)
import Data.Char (isSpace)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text)
import Data.Text qualified as T (dropWhile, null)
import Data.Text.Read (Reader)

groupConsecutiveBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupConsecutiveBy f xs = chunk id $ zip xs $ True : zipWith f xs (drop 1 xs)
  where
    chunk k [] = filter (not . null) [k []]
    chunk k ((x, False) : rest) = k [] : chunk (x :) rest
    chunk k ((x, True) : rest) = chunk (k . (x :)) rest

readEntire :: Reader a -> Text -> Either String a
readEntire reader input = do
  (a, t) <- reader input
  if T.null t then Right a else Left "incomplete read"

readMany :: Reader a -> Reader [a]
readMany reader = pure . readMany' id
  where
    readMany' k input =
      either (const (k [], input)) (uncurry $ readMany' . (.) k . (:)) . reader $
        T.dropWhile isSpace input

readSome :: Reader a -> Reader (NonEmpty a)
readSome reader input = do
  (a, input') <- reader input
  first (a :|) <$> readMany reader input'
