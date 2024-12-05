-- |
-- Module:         Day5
-- Description:    <https://adventofcode.com/2024/day/5 Day 5: Print Queue>
module Day5 (part1, part2) where

import Data.List (tails)
import Data.Set (Set)
import Data.Set qualified as Set (fromList, notMember)
import Data.String (IsString)
import Data.Text (Text)
import Data.Vector.Generic (Vector)
import Data.Vector.Generic qualified as V (fromList, length, modify, (!))
import Data.Vector.Generic.Mutable qualified as MV (length, read, write)
import Data.Vector.Unboxed qualified as UV (Vector)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec, ParseErrorBundle, Stream (Token, Tokens), parse, sepEndBy, sepEndBy1, skipMany)
import Text.Megaparsec.Char (char, newline)
import Text.Megaparsec.Char.Lexer (decimal)

parser :: (MonadParsec e s m, IsString (Tokens s), Token s ~ Char, Num a, Ord a) => m (Set (a, a), [[a]])
parser =
  (,) . Set.fromList
    <$> ((,) <$> decimal <* char '|' <*> decimal) `sepEndBy` newline
    <* skipMany newline
    <*> (decimal `sepEndBy1` char ',') `sepEndBy` newline

part1 :: Text -> Either (ParseErrorBundle Text Void) Int
part1 input = do
  (deps, updates) <- parse parser "" input
  pure . sum $
    [ update !! (length update `div` 2)
    | update <- updates,
      and [(b, a) `Set.notMember` deps | a : rest <- tails update, b <- rest]
    ]

part2 :: Text -> Either (ParseErrorBundle Text Void) Int
part2 input = do
  (deps, updates) <- parse parser "" input
  let ok a b = (b, a) `Set.notMember` deps
  pure . sum $
    [ pages V.! (V.length pages `div` 2)
    | update <- V.fromList @UV.Vector @Int <$> updates,
      let pages = sort' ok update,
      update /= pages
    ]

sort' :: (Vector v a) => (a -> a -> Bool) -> v a -> v a
sort' ok = V.modify $ \v ->
  let go i j
        | j < MV.length v = do
            x <- MV.read v i
            y <- MV.read v j
            if ok x y
              then go i (j + 1)
              else do
                MV.write v i y
                MV.write v j x
                go i (i + 1)
        | i < MV.length v = go (i + 1) (i + 2)
        | otherwise = pure ()
   in go 0 1
