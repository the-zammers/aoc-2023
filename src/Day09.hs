module Day09 where

import Paths_aoc2023 (getDataFileName)

day09 :: IO ()
day09 = do
  contents <- init . lines <$> (getDataFileName "day09.txt" >>= readFile)
  putStrLn $ unlines contents
