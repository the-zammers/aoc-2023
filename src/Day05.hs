module Day05 where

import Paths_aoc2023 (getDataFileName)

day05 :: IO ()
day05 = do
  contents <- init . lines <$> (getDataFileName "day05.txt" >>= readFile)
  putStrLn $ unlines contents
