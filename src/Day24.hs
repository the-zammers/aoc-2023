module Day24 where

import Paths_aoc2023 (getDataFileName)

day24 :: IO ()
day24 = do
  contents <- init . lines <$> (getDataFileName "day24.txt" >>= readFile)
  putStrLn $ unlines contents
