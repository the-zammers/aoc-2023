module Day23 where

import Paths_aoc2023 (getDataFileName)

day23 :: IO ()
day23 = do
  contents <- init . lines <$> (getDataFileName "day23.txt" >>= readFile)
  putStrLn $ unlines contents
