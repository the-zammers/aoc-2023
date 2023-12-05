module Day15 where

import Paths_aoc2023 (getDataFileName)

day15 :: IO ()
day15 = do
  contents <- init . lines <$> (getDataFileName "day15.txt" >>= readFile)
  putStrLn $ unlines contents
