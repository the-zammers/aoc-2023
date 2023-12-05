module Day19 where

import Paths_aoc2023 (getDataFileName)

day19 :: IO ()
day19 = do
  contents <- init . lines <$> (getDataFileName "day19.txt" >>= readFile)
  putStrLn $ unlines contents
