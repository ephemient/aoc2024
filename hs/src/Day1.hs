{-|
Module:         Day1
Description:    <https://adventofcode.com/2024/day/1 Day 1: Historian Hysteria>
-}
module Day1 (part1, part2) where

import Common (readEntire)
import Data.Function (on)
import qualified Data.IntMap as IntMap (fromListWith, findWithDefault)
import Data.List (sort, transpose)
import Data.Text (Text)
import qualified Data.Text as T (lines, words)
import qualified Data.Text.Read as T (decimal)

parse :: Text -> Either String [[Int]]
parse = fmap transpose . mapM (mapM (readEntire T.decimal) . T.words) . T.lines

part1 :: Text -> Either String Int
part1 input = case parse input of
    Left err -> Left err
    Right [as, bs] -> pure $ sum $ abs <$> (zipWith (-) `on` sort) as bs
    _ -> Left "no parse"

part2 :: Text -> Either String Int
part2 input = case parse input of
    Left err -> Left err
    Right [as, bs] ->
        let cs = IntMap.fromListWith (($!) . (+)) [(b, 1) | b <- bs]
        in pure $ sum [a * IntMap.findWithDefault 0 a cs | a <- as]
    _ -> Left "no parse"
