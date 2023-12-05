module Day11 where

import Paths_aoc2023 (getDataFileName)

day11 :: IO ()
day11 = do
  contents <- init . lines <$> (getDataFileName "day11.txt" >>= readFile)
  putStrLn $ unlines contents
