module Day16 where

import Paths_aoc2023 (getDataFileName)

day16 :: IO ()
day16 = do
  contents <- init . lines <$> (getDataFileName "day16.txt" >>= readFile)
  putStrLn $ unlines contents
