{-# LANGUAGE NondecreasingIndentation #-}

module Main (main) where

import Control.Monad (ap, when)
import Data.Bifunctor (bimap)
import Data.Foldable (find)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.IO qualified as TIO (readFile)
import Day1 qualified (part1, part2)
import Day10 qualified (part1, part2)
import Day11 qualified (part1, part2)
import Day12 qualified (part1, part2)
import Day13 qualified (part1, part2)
import Day14 qualified (part1, part2)
import Day15 qualified (part1, part2)
import Day16 qualified (part1, part2)
import Day17 qualified (part1, part2)
import Day18 qualified (part1, part2)
import Day19 qualified (solve)
import Day2 qualified (part1, part2)
import Day20 qualified (solve)
import Day3 qualified (part1, part2)
import Day4 qualified (part1, part2)
import Day5 qualified (part1, part2)
import Day6 qualified (part1, part2)
import Day7 qualified (part1, part2)
import Day8 qualified (part1, part2)
import Day9 qualified (part1, part2)
import System.Environment (getArgs, lookupEnv)
import System.FilePath (combine)
import Text.Megaparsec (errorBundlePretty)

getDayInput :: Int -> IO Text
getDayInput i = do
  dataDir <- fromMaybe "." . find (not . null) <$> lookupEnv "AOC2024_DATADIR"
  TIO.readFile . combine dataDir $ "day" ++ show i ++ ".txt"

run :: Int -> (a -> IO ()) -> [Text -> a] -> IO ()
run = run' `ap` show

run' :: Int -> String -> (a -> IO ()) -> [Text -> a] -> IO ()
run' day name showIO funcs = do
  args <- getArgs
  when (null args || name `elem` args) $ do
    putStrLn $ "Day " ++ name
    contents <- getDayInput day
    mapM_ (showIO . ($ contents)) funcs
    putStrLn ""

main :: IO ()
main = do
  run 1 (either fail print) [Day1.part1, Day1.part2]
  run 2 (either fail print) [Day2.part1, Day2.part2]
  run 3 print [Day3.part1, Day3.part2]
  run 4 print [Day4.part1, Day4.part2]
  run 5 (either (fail . errorBundlePretty) print) [Day5.part1, Day5.part2]
  run 6 print [Day6.part1, Day6.part2]
  run 7 (either fail print) [Day7.part1, Day7.part2]
  run 8 print [Day8.part1, Day8.part2]
  run 9 print [Day9.part1, Day9.part2]
  run 10 print [Day10.part1, Day10.part2]
  run 11 (either fail print) [Day11.part1, Day11.part2]
  run 12 print [Day12.part1, Day12.part2]
  run 13 (either (fail . errorBundlePretty) print) [Day13.part1, Day13.part2]
  run 14 (either (fail . errorBundlePretty) print) [Day14.part1, Day14.part2]
  run 15 (either fail print) [Day15.part1, Day15.part2]
  run 16 (maybe (fail "error") print) [Day16.part1, Day16.part2]
  run 17 (either (fail . errorBundlePretty) $ putStrLn . intercalate "," . map show) [Day17.part1, fmap (: []) . Day17.part2]
  run 18 (either fail putStrLn) [fmap show . Day18.part1, fmap (uncurry $ (. (',' :) . show) . shows) . Day18.part2]
  run 19 (uncurry (>>) . bimap print print) [Day19.solve]
  run 20 print [Day20.solve 2 100, Day20.solve 20 100]
