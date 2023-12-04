module Day02 where

import Paths_aoc2023 (getDataFileName)
import Data.List.Split (splitOneOf, chunksOf)

day02 :: IO ()
day02 = do
  contents <- init . lines <$> (getDataFileName "day02.txt" >>= readFile)
  let formatted = map format contents
  --putStrLn $ unlines $ map show formatted
  print $ solveA formatted
  print $ solveB formatted

format :: String -> (Int, [[Int]])
format s = (read $ drop 5 $ head parts, map (foldr1 (zipWith (+)) . map format' . chunksOf 2 . words) (tail parts))
  where parts = splitOneOf ":;" s

format' :: [String] -> [Int]
format' [num,col] = case col!!0 of
  'r' -> [x, 0, 0]
  'g' -> [0, x, 0]
  'b' -> [0, 0, x]
  where x = read num
format' _ = repeat 0

solveA :: [(Int, [[Int]])] -> Int
solveA games = sum $ map fst $ filter (all (and . zipWith (>=) [12..]) . snd) games

solveB :: [(Int, [[Int]])] -> Int
solveB games = sum $ map (product . foldr1 (zipWith max) . snd) games
