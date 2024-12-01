module Main (main) where

import Control.Arrow ((>>>))
import Criterion.Main (bench, bgroup, defaultMain, env, envWithCleanup, nf)
import Data.Foldable (find)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text.IO as TIO (readFile)
import qualified Day1 (part1, part2)
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
main = defaultMain
  [ env (getDayInput 1) $ \input -> bgroup "Day 1"
      [ bench "part 1" $ nf Day1.part1 input
      , bench "part 2" $ nf Day1.part2 input
      ]
  ]
