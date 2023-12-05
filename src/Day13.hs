module Day13 where

import Paths_aoc2023 (getDataFileName)

day13 :: IO ()
day13 = do
  contents <- init . lines <$> (getDataFileName "day13.txt" >>= readFile)
  putStrLn $ unlines contents
