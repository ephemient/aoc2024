-- |
-- Module:         Day5
-- Description:    <https://adventofcode.com/2024/day/5 Day 5: Print Queue>
module Day5 (part1, part2) where

import Control.Arrow (second)
import Data.IntMap qualified as IntMap (findWithDefault, fromList, (!?))
import Data.IntMap.Strict qualified as IntMap (fromListWith)
import Data.IntSet qualified as IntSet (empty, member, singleton, toList, union)
import Data.List (sortBy)
import Data.Maybe (fromJust, fromMaybe)
import Data.String (IsString)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec, ParseErrorBundle, Stream (Token, Tokens), parse, sepEndBy, sepEndBy1, skipMany)
import Text.Megaparsec.Char (char, newline)
import Text.Megaparsec.Char.Lexer (decimal)

parser :: (MonadParsec e s m, IsString (Tokens s), Token s ~ Char, Num a) => m ([(a, a)], [[a]])
parser =
  (,)
    <$> ((,) <$> decimal <* char '|' <*> decimal) `sepEndBy` newline
    <* skipMany newline
    <*> (decimal `sepEndBy1` char ',') `sepEndBy` newline

part1 :: Text -> Either (ParseErrorBundle Text Void) Int
part1 input = do
  (deps, updates) <- parse parser "" input
  pure $
    sum
      [ update !! (length update `div` 2)
      | update <- updates,
        let order = IntMap.fromList @Int $ zip update [0 ..],
        and
          [ fromMaybe True $ (<) <$> order IntMap.!? a <*> order IntMap.!? b
          | (a, b) <- deps
          ]
      ]

part2 :: Text -> Either (ParseErrorBundle Text Void) Int
part2 input = do
  (deps, updates) <- parse parser "" input
  let deps' = IntMap.fromListWith IntSet.union $ second IntSet.singleton <$> deps
      tryCompare a b
        | a == b = Just EQ
        | b `IntSet.member` (IntMap.findWithDefault IntSet.empty a deps') = Just LT
        | a `IntSet.member` (IntMap.findWithDefault IntSet.empty b deps') = Just GT
        | otherwise =
            mconcat (tryCompare a <$> maybe [] IntSet.toList (deps' IntMap.!? b))
              <> fmap (compare EQ) (mconcat (tryCompare b <$> maybe [] IntSet.toList (deps' IntMap.!? a)))
      compare' a b = fromJust $ tryCompare a b
  pure $
    sum
      [ update' !! (length update `div` 2)
      | update <- updates,
        let update' = sortBy compare' update,
        update /= update'
      ]
