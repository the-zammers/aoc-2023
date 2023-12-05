module Day18 where

import Paths_aoc2023 (getDataFileName)

day18 :: IO ()
day18 = do
  contents <- init . lines <$> (getDataFileName "day18.txt" >>= readFile)
  putStrLn $ unlines contents
