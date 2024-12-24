{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module:         Day24
-- Description:    <https://adventofcode.com/2024/day/24 Day 24: Crossed Wires>
module Day24 (part1) where

import Control.Applicative ((<|>))
import Control.Monad (join, liftM2)
import Data.Bifunctor (bimap)
import Data.Bits ((.&.), (.^.), (.|.))
import Data.Char (isAlphaNum)
import Data.Functor (($>))
import Data.Map qualified as Map (dropWhileAntitone, fromList, takeWhileAntitone, (!?))
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as T (singleton)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec, ParseErrorBundle, Token, Tokens, choice, parse, sepEndBy, takeWhile1P)
import Text.Megaparsec.Char (newline, string)
import Text.Megaparsec.Char.Lexer qualified as L (decimal)

data Expr a = Expr a :&: Expr a | Expr a :|: Expr a | Expr a :^: Expr a | Literal a

parser :: (MonadParsec e s m, IsString (Tokens s), Token s ~ Char, Num a) => m ([(Tokens s, a)], [(Tokens s, Expr (Tokens s))])
parser = (,) <$> (initial `sepEndBy` newline) <* newline <*> (wire `sepEndBy` newline)
  where
    initial = (,) <$> takeWhile1P Nothing isAlphaNum <* string ": " <*> L.decimal
    wire =
      flip (,)
        <$> ( (flip ($) . Literal <$> takeWhile1P Nothing isAlphaNum)
                <*> choice [string " AND " $> (:&:), string " OR " $> (:|:), string " XOR " $> (:^:)]
                <*> (Literal <$> takeWhile1P Nothing isAlphaNum)
            )
        <* string " -> "
        <*> takeWhile1P Nothing isAlphaNum

part1 :: Text -> Either (ParseErrorBundle Text Void) (Maybe Int)
part1 input = do
  (givens, wires) <- bimap Map.fromList Map.fromList <$> parse parser "" input
  let values = fmap eval wires
      eval (a :&: b) = (.&.) <$> eval a <*> eval b
      eval (a :|: b) = (.|.) <$> eval a <*> eval b
      eval (a :^: b) = (.^.) <$> eval a <*> eval b
      eval (Literal a) = givens Map.!? a <|> join (values Map.!? a)
  pure $
    foldr (liftM2 . flip $ (+) . (*) 2) (Just 0) $
      Map.takeWhileAntitone (< T.singleton (succ 'z')) $
        Map.dropWhileAntitone (< "z") values
