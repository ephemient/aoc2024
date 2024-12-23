{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- |
-- Module:         Day23
-- Description:    <https://adventofcode.com/2024/day/23 Day 23: LAN Party>
module Day23 (part1, part2) where

import Data.Foldable (maximumBy)
import Data.Map (Map)
import Data.Map qualified as Map (restrictKeys, toList, (!))
import Data.Map.Strict qualified as Map (fromListWith)
import Data.Ord (comparing)
import Data.Set (Set)
import Data.Set qualified as Set (empty, insert, member, singleton, size, toList)
import Data.Text (Text, pattern (:<))
import Data.Text qualified as T (breakOn, intercalate, isPrefixOf, lines)

parse :: Text -> Map Text (Set Text)
parse input = Map.fromListWith (<>) $ do
  (a, '-' :< b) <- T.breakOn "-" <$> T.lines input
  [(min a b, Set.singleton $ max a b), (max a b, Set.empty)]

part1 :: Text -> Int
part1 input =
  length
    [ ()
    | (a, bs) <- Map.toList graph,
      b <- Set.toList bs,
      c <- Set.toList $ graph Map.! b,
      c `Set.member` bs,
      "t" `T.isPrefixOf` a || "t" `T.isPrefixOf` b || "t" `T.isPrefixOf` c
    ]
  where
    graph = parse input

part2 :: Text -> Text
part2 input = T.intercalate "," $ Set.toList $ go Set.empty graph
  where
    graph = parse input
    go acc subgraph =
      maximumBy (comparing Set.size) $
        acc : [go (Set.insert a acc) $ subgraph `Map.restrictKeys` bs | (a, bs) <- Map.toList subgraph]
