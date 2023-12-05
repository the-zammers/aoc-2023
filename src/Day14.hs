module Day14 where

import Paths_aoc2023 (getDataFileName)

day14 :: IO ()
day14 = do
  contents <- init . lines <$> (getDataFileName "day14.txt" >>= readFile)
  putStrLn $ unlines contents
