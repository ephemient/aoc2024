module Main (main) where

import Control.Arrow ((>>>))
import Criterion.Main (bench, bgroup, defaultMain, env, envWithCleanup, nf)
import Data.Foldable (find)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.IO qualified as TIO (readFile)
import Day1 qualified (part1, part2)
import Day2 qualified (part1, part2)
import Day3 qualified (part1, part2)
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
          ]
    ]
