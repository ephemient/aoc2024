{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module:         Day3
-- Description:    <https://adventofcode.com/2024/day/3 Day 3: Mull It Over>
module Day3 (part1, part2) where

import Control.Monad (filterM)
import Control.Monad.State (MonadState (get, put), evalState)
import Data.Either (rights)
import Data.Functor (($>))
import Data.String (IsString)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec (try), ParseErrorBundle, Stream (Token, Tokens), anySingle, between, many, parse, skipManyTill, (<|>))
import Text.Megaparsec.Char (char, string)
import Text.Megaparsec.Char.Lexer qualified as L (decimal)

mul :: (MonadParsec e s m, IsString (Tokens s), Token s ~ Char, Num a) => m a
mul = between (string "mul(") (string ")") $ (*) <$> L.decimal <* char ',' <*> L.decimal

parser1 :: (MonadParsec e s m, IsString (Tokens s), Token s ~ Char, Num a) => m [a]
parser1 = many $ try $ skipManyTill anySingle $ try mul

parser2 :: (MonadParsec e s m, IsString (Tokens s), Token s ~ Char, Num a) => m [Either Bool a]
parser2 = many $ try $ skipManyTill anySingle $ Right <$> try mul <|> string "do()" $> Left True <|> string "don't()" $> Left False

part1 :: Text -> Either (ParseErrorBundle Text Void) Int
part1 = fmap sum . parse parser1 ""

part2 :: Text -> Either (ParseErrorBundle Text Void) Int
part2 = fmap (sum . rights . flip evalState True . filterM (either (($> False) . put) $ const get)) . parse parser2 ""
