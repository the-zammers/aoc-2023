module DayXY where

import Paths_aoc2023 (getDataFileName)

dayXY :: IO ()
dayXY = do
  contents <- init . lines <$> (getDataFileName "dayXY.txt" >>= readFile)
  putStrLn $ unlines contents
