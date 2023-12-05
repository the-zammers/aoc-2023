module Day21 where

import Paths_aoc2023 (getDataFileName)

day21 :: IO ()
day21 = do
  contents <- init . lines <$> (getDataFileName "day21.txt" >>= readFile)
  putStrLn $ unlines contents
