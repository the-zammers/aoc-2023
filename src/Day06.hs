module Day06 where

import Paths_aoc2023 (getDataFileName)

day06 :: IO ()
day06 = do
  contents <- init . lines <$> (getDataFileName "day06.txt" >>= readFile)
  putStrLn $ unlines contents
