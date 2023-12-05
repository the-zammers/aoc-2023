module Day22 where

import Paths_aoc2023 (getDataFileName)

day22 :: IO ()
day22 = do
  contents <- init . lines <$> (getDataFileName "day22.txt" >>= readFile)
  putStrLn $ unlines contents
