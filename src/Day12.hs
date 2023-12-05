module Day12 where

import Paths_aoc2023 (getDataFileName)

day12 :: IO ()
day12 = do
  contents <- init . lines <$> (getDataFileName "day12.txt" >>= readFile)
  putStrLn $ unlines contents
