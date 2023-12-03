module Day03 where

import Paths_aoc2023 (getDataFileName)
import Data.Char (isDigit)
import Data.List (group)
import Data.Maybe (catMaybes)
import Data.Vector ((!?))
import qualified Data.Vector as V

type Schematic = V.Vector (V.Vector Int)

day03 :: IO ()
day03 = do
  contents <- init . lines <$> (getDataFileName "day03.txt" >>= readFile)
  let formatted = format contents
  print $ solveA $ formatted
  print $ solveB $ formatted


format :: [[Char]] -> Schematic
format x = V.fromList $ map (V.fromList . format') x

format' :: [Char] -> [Int]
format' [] = []
format' x
  | isDigit $ head x = replicate (length num) (read num) ++ format' rest
  | (=='.') $ head x =  0 : format' (tail x)
  | (=='*') $ head x = -1 : format' (tail x)
  | otherwise =        -2 : format' (tail x)
  where (num, rest) = span isDigit x

solveA :: Schematic -> Int
solveA v = sum $ process <$> (isPart $ indices v)
  where isPart = filter ((< Just 0) . safeIndex v)
        process p = sum $ adjacents v p

solveB :: Schematic -> Int
solveB v = sum $ process <$> (isGear $ indices v)
  where isGear = filter ((== Just (-1)) . safeIndex v)
        process p = case length (adjacents v p) of
                      2 -> product $ adjacents v p
                      _ -> 0

indices :: Schematic -> [(Int, Int)]
indices v = (,) <$> [0..length v-1] <*>[0..length (V.head v)-1]

adjacents :: Schematic -> (Int, Int) -> [Int]
adjacents v p = (map head . group) $ filter (>0) $ catMaybes $ kernel v p

kernel :: Schematic -> (Int, Int) -> [Maybe Int]
kernel v (a,b) = map (safeIndex v) $ filter (/=(a,b)) $ (,) <$> [a-1..a+1] <*> [b-1..b+1]

safeIndex :: Schematic -> (Int, Int) -> Maybe Int
safeIndex v (a,b) = v !? a >>= (!? b)
