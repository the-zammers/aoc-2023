module Day09 where

import Paths_aoc2023 (getDataFileName)
import Control.Arrow ((&&&))

type Point = (Integer, Integer)

day09 :: IO ()
day09 = do
  contents <- init . lines <$> (getDataFileName "day09.txt" >>= readFile)
  print $ solve partA $ format contents -- 2098530125
  print $ solve partB $ format contents -- 1016

format :: [String] -> [[Point]]
format = map $ zip [0..] . map read . words

solve :: ([Point] -> Integer) -> [[Point]] -> Integer
solve part = sum . map (\ls -> interpolate ls (part ls)) -- lambda left for clarity

partA, partB :: [Point] -> Integer
partA = length'
partB = const (-1)

interpolate :: [Point] -> Integer -> Integer
interpolate ps x = sum $ map (uncurry ((*) . basis)) ps
  where basis j = uncurry div $ (product . map (x-)) &&& (product . map (j-)) $ filter (/=j) $ [0 .. length' ps - 1]

length' :: Foldable t => t a -> Integer
length' = toInteger . length
