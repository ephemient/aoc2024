module Main (main) where

import Control.Arrow ((>>>))
import Criterion.Main (bench, bgroup, defaultMain, env, envWithCleanup, nf)
import Data.Foldable (find)
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
import Day2 qualified (part1, part2)
import Day3 qualified (part1, part2)
import Day4 qualified (part1, part2)
import Day5 qualified (part1, part2)
import Day6 qualified (part1, part2)
import Day7 qualified (part1, part2)
import Day8 qualified (part1, part2)
import Day9 qualified (part1, part2)
import System.Environment.Blank (getEnv, setEnv, unsetEnv)
import System.FilePath (combine)

setTrace :: String -> IO (Maybe String)
setTrace value = getEnv "TRACE" <* setEnv "TRACE" value True

unsetTrace :: Maybe String -> IO ()
unsetTrace = maybe (unsetEnv "TRACE") (setEnv "TRACE" `flip` True)

getDayInput :: Int -> IO Text
getDayInput i = do
  dataDir <- fromMaybe "." . find (not . null) <$> getEnv "AOC2024_DATADIR"
  TIO.readFile . combine dataDir $ "day" ++ show i ++ ".txt"

main :: IO ()
main =
  defaultMain
    [ env (getDayInput 1) $ \input ->
        bgroup
          "Day 1"
          [ bench "part 1" $ nf Day1.part1 input,
            bench "part 2" $ nf Day1.part2 input
          ],
      env (getDayInput 2) $ \input ->
        bgroup
          "Day 2"
          [ bench "part 1" $ nf Day2.part1 input,
            bench "part 2" $ nf Day2.part2 input
          ],
      env (getDayInput 3) $ \input ->
        bgroup
          "Day 3"
          [ bench "part 1" $ nf Day3.part1 input,
            bench "part 2" $ nf Day3.part2 input
          ],
      env (getDayInput 4) $ \input ->
        bgroup
          "Day 4"
          [ bench "part 1" $ nf Day4.part1 input,
            bench "part 2" $ nf Day4.part2 input
          ],
      env (getDayInput 5) $ \input ->
        bgroup
          "Day 5"
          [ bench "part 1" $ nf Day5.part1 input,
            bench "part 2" $ nf Day5.part2 input
          ],
      env (getDayInput 6) $ \input ->
        bgroup
          "Day 6"
          [ bench "part 1" $ nf Day6.part1 input,
            bench "part 2" $ nf Day6.part2 input
          ],
      env (getDayInput 7) $ \input ->
        bgroup
          "Day 7"
          [ bench "part 1" $ nf Day7.part1 input,
            bench "part 2" $ nf Day7.part2 input
          ],
      env (getDayInput 8) $ \input ->
        bgroup
          "Day 8"
          [ bench "part 1" $ nf Day8.part1 input,
            bench "part 2" $ nf Day8.part2 input
          ],
      env (getDayInput 9) $ \input ->
        bgroup
          "Day 9"
          [ bench "part 1" $ nf Day9.part1 input,
            bench "part 2" $ nf Day9.part2 input
          ],
      env (getDayInput 10) $ \input ->
        bgroup
          "Day 10"
          [ bench "part 1" $ nf Day10.part1 input,
            bench "part 2" $ nf Day10.part2 input
          ],
      env (getDayInput 11) $ \input ->
        bgroup
          "Day 11"
          [ bench "part 1" $ nf Day11.part1 input,
            bench "part 2" $ nf Day11.part2 input
          ],
      env (getDayInput 12) $ \input ->
        bgroup
          "Day 12"
          [ bench "part 1" $ nf Day12.part1 input,
            bench "part 2" $ nf Day12.part2 input
          ],
      env (getDayInput 13) $ \input ->
        bgroup
          "Day 13"
          [ bench "part 1" $ nf Day13.part1 input,
            bench "part 2" $ nf Day13.part2 input
          ],
      env (getDayInput 14) $ \input ->
        bgroup
          "Day 14"
          [ bench "part 1" $ nf Day14.part1 input,
            bench "part 2" $ nf Day14.part2 input
          ],
      env (getDayInput 15) $ \input ->
        bgroup
          "Day 15"
          [ bench "part 1" $ nf Day15.part1 input,
            bench "part 2" $ nf Day15.part2 input
          ],
      env (getDayInput 16) $ \input ->
        bgroup
          "Day 16"
          [ bench "part 1" $ nf Day16.part1 input,
            bench "part 2" $ nf Day16.part2 input
          ],
      env (getDayInput 17) $ \input ->
        bgroup
          "Day 17"
          [ bench "part 1" $ nf Day17.part1 input,
            bench "part 2" $ nf Day17.part2 input
          ],
      env (getDayInput 18) $ \input ->
        bgroup
          "Day 18"
          [ bench "part 1" $ nf Day18.part1 input,
            bench "part 2" $ nf Day18.part2 input
          ]
    ]
