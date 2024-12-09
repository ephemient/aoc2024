{-# LANGUAGE NondecreasingIndentation #-}

module Main (main) where

import Control.Monad (ap, when)
import Data.Foldable (find)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.IO qualified as TIO (readFile)
import Day1 qualified (part1, part2)
import Day10 qualified (part1, part2)
import Day11 qualified (part1, part2)
import Day2 qualified (part1, part2)
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
