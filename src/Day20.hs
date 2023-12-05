module Day20 where

import Paths_aoc2023 (getDataFileName)

day20 :: IO ()
day20 = do
  contents <- init . lines <$> (getDataFileName "day20.txt" >>= readFile)
  putStrLn $ unlines contents
