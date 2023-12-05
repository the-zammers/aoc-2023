module Day08 where

import Paths_aoc2023 (getDataFileName)

day08 :: IO ()
day08 = do
  contents <- init . lines <$> (getDataFileName "day08.txt" >>= readFile)
  putStrLn $ unlines contents
