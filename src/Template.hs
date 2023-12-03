module Day0x where

import Paths_aoc2023 (getDataFileName)

day0x :: IO ()
day0x = do
  contents <- init . lines <$> (getDataFileName "day0x.txt" >>= readFile)

