module Day12 where

-- only finished the first part, tried to solve the second part, kept having trouble,
-- discovered that glguy's solution just doesn't work for me (not enough memory),
-- and then mistakenly deleted the first part's work, too

import Paths_aoc2023 (getDataFileName)

day12 :: IO ()
day12 = do
  contents <- init . lines <$> (getDataFileName "day12a.txt" >>= readFile)
  putStrLn $ unlines contents
