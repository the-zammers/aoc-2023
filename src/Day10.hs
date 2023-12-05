module Day10 where

import Paths_aoc2023 (getDataFileName)

day10 :: IO ()
day10 = do
  contents <- init . lines <$> (getDataFileName "day10.txt" >>= readFile)
  putStrLn $ unlines contents
