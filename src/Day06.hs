module Day06 where

import Paths_aoc2023 (getDataFileName)
import Data.Tuple.Extra (both)
import Data.Char (isDigit)

day06 :: IO ()
day06 = do
  contents <- break (=='\n') <$> (getDataFileName "day06.txt" >>= readFile)
  print $ solve $ format partA contents
  print $ solve $ format partB contents

format :: (String -> String) -> (String, String) -> [(Double, Double)]
format f = uncurry zip . both (map read . words . f . dropWhile (/=' '))

partA, partB :: String -> String
partA = id
partB = filter isDigit

solve :: (Floating a, RealFrac a) => [(a,a)] -> Int
solve = product . map one
  where one (t, d) = let discrim = sqrt (t**2 - 4*d) / 2 in 
                     ceiling (t/2 + discrim) - floor (t/2 - discrim) - 1
