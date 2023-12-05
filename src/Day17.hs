module Day17 where

import Paths_aoc2023 (getDataFileName)

day17 :: IO ()
day17 = do
  contents <- init . lines <$> (getDataFileName "day17.txt" >>= readFile)
  putStrLn $ unlines contents
