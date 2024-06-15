module Day13 where

import Paths_aoc2023 (getDataFileName)
import Data.List.Split
import Data.List

day13 :: IO ()
day13 = do
  contents <- unlines . init . lines <$> (getDataFileName "day13a.txt" >>= readFile)
  let input = format contents
  print $ sum (map vertLine $ format contents) + 100 * sum (map horzLine $ format contents)
  print $ map vertLine input
  print $ map horzLine input
  let test = input!!0
  let test2 = reverse $ input!!1
  print $ test
  print $ maximum (overlap test2 (reverse test2)) `div` 2
  print $ negate (maximum $ overlap (reverse test2) test2) `div` 2

format :: String -> [ [[Char]] ]
format xs = splitOn [""] $ lines xs

vertLine = horzLine . transpose

horzLine x
  | uppish > 0 && downish < 0 = (`div` 2) $ if abs (uppish - midpoint) < abs ((length x + downish) - (length x + 1) `div` 2) then uppish else (length x + downish)
  | uppish > 0 = (`div` 2) $ uppish
  | downish < 0 = (`div` 2) $ length x + downish
  | otherwise = 0
  where uppish  = (maximum $ overlap x (reverse x))
        downish = negate (maximum $ overlap (reverse x) x)
        midpoint = length x `div` 2

overlap :: (Eq a) => [a] -> [a] -> [Int]
overlap a b = map length $ intersect (inits a) (tails b)
