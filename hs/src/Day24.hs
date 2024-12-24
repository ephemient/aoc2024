{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- |
-- Module:         Day24
-- Description:    <https://adventofcode.com/2024/day/24 Day 24: Crossed Wires>
module Day24 (part1, part2) where

import Control.Applicative ((<|>))
import Control.Monad (foldM, join, liftM2)
import Data.Bifunctor (bimap)
import Data.Bits (Bits, (.&.), (.^.), (.|.))
import Data.Char (isAlphaNum)
import Data.Functor (($>))
import Data.List (sort)
import Data.Map (Map)
import Data.Map qualified as Map (dropWhileAntitone, fromList, takeWhileAntitone, (!?))
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set (empty, fromList, toList)
import Data.String (IsString)
import Data.Text (Text, pattern (:<))
import Data.Text qualified as T (intercalate, singleton, stripPrefix)
import Data.Tuple (swap)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec, ParseErrorBundle, Token, Tokens, choice, parse, sepEndBy, takeWhile1P)
import Text.Megaparsec.Char (newline, string)
import Text.Megaparsec.Char.Lexer qualified as L (decimal)

data Expr a = a :&: a | a :|: a | a :^: a deriving (Eq, Ord)

parser :: (MonadParsec e s m, IsString (Tokens s), Token s ~ Char, Num a) => m ([(Tokens s, a)], [(Tokens s, Expr (Tokens s))])
parser = (,) <$> (initial `sepEndBy` newline) <* newline <*> (wire `sepEndBy` newline)
  where
    initial = (,) <$> takeWhile1P Nothing isAlphaNum <* string ": " <*> L.decimal
    wire =
      flip (,)
        <$> do
          lhs <- takeWhile1P Nothing isAlphaNum
          op <- choice [string " AND " $> (:&:), string " OR " $> (:|:), string " XOR " $> (:^:)]
          rhs <- takeWhile1P Nothing isAlphaNum
          pure $ min lhs rhs `op` max lhs rhs
        <* string " -> "
        <*> takeWhile1P Nothing isAlphaNum

eval :: (Ord k, Bits a) => Map k (Expr k) -> (k -> Maybe a) -> Map k (Maybe a)
eval wires f = values'
  where
    values' = fmap eval' wires
    eval' (a :&: b) = (.&.) <$> eval'' a <*> eval'' b
    eval' (a :|: b) = (.|.) <$> eval'' a <*> eval'' b
    eval' (a :^: b) = (.^.) <$> eval'' a <*> eval'' b
    eval'' a = f a <|> join (values' Map.!? a)

part1 :: Text -> Either (ParseErrorBundle Text Void) (Maybe Int)
part1 input = do
  (givens, wires) <- bimap Map.fromList Map.fromList <$> parse parser "" input
  let values = eval wires (givens Map.!?)
  pure $
    foldr (liftM2 . flip $ (+) . (*) 2) (Just 0) $
      Map.takeWhileAntitone (< T.singleton (succ 'z')) $
        Map.dropWhileAntitone (< "z") values

part2 :: Text -> Either (ParseErrorBundle Text Void) (Maybe Text)
part2 input = do
  (_ :: [(Text, Int)], wires) <- parse parser "" input
  let wires' = Map.fromList $ swap <$> wires
  pure $ fmap finish $ foldM go (Set.empty, Nothing, wires') $ sort $ mapMaybe (T.stripPrefix "z" . fst) wires
  where
    go (acc, carry, wires) suffix
      | Just carry' <- carry = case wires Map.!? (x :^: y) of
          Nothing -> if carry' /= z then swizzle carry' z else pure (acc, Nothing, wires)
          Just halfAdd -> case wires Map.!? (min halfAdd carry' :^: max halfAdd carry') of
            Nothing -> do
              halfAdd' <- wires Map.!? (x :&: y)
              swizzle halfAdd halfAdd'
            Just fullAdd ->
              if fullAdd /= z
                then swizzle fullAdd z
                else
                  let carry'' = do
                        overflow1 <- wires Map.!? (x :&: y)
                        overflow2 <- wires Map.!? (min halfAdd carry' :&: max halfAdd carry')
                        wires Map.!? (min overflow1 overflow2 :|: max overflow1 overflow2)
                   in pure (acc, carry'', wires)
      | "z00" <- z = do
          add <- wires Map.!? (x :^: y)
          if add /= z then swizzle add z else pure (acc, wires Map.!? (x :&: y), wires)
      | otherwise = Nothing
      where
        x = 'x' :< suffix
        y = 'y' :< suffix
        z = 'z' :< suffix
        swizzle a b = go (acc <> Set.fromList [a, b], carry, fmap f wires) suffix
          where
            f c | c == a = b | c == b = a | otherwise = c
    finish (acc, _, _) = T.intercalate "," $ Set.toList acc
