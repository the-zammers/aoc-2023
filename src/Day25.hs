module Day25 where

import Paths_aoc2023 (getDataFileName)

day25 :: IO ()
day25 = do
  contents <- init . lines <$> (getDataFileName "day25.txt" >>= readFile)
  putStrLn $ unlines contents
