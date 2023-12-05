module Day07 where

import Paths_aoc2023 (getDataFileName)

day07 :: IO ()
day07 = do
  contents <- init . lines <$> (getDataFileName "day07.txt" >>= readFile)
  putStrLn $ unlines contents
