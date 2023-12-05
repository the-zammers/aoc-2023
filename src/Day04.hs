module Day04 where

import Paths_aoc2023 (getDataFileName)
import Data.List.Split (splitOneOf)
import Data.Bifunctor (first)

type Card = ([Int], [Int])

day04 :: IO ()
day04 = do
  contents <- init . lines <$> (getDataFileName "day04.txt" >>= readFile)
  let formatted = map format contents
  print $ solveA formatted
  print $ solveB formatted

format :: String -> (Card, Int)
format x = ((card!!0, card!!1), 1)
  where card = map (map read . words) $ drop 1 $ splitOneOf ":|" x

solveA :: [(Card, Int)] -> Int
solveA = sum . map ((`div` 2) . (2 ^) . matches . fst)

solveB :: [(Card, Int)] -> Int
solveB [] = 0
solveB x = snd (head x) + solveB (updateRest x)

updateRest :: [(Card, Int)] -> [(Card, Int)]
updateRest [] = [] -- for completness' sake
updateRest y@((x,c):_) = uncurry (++) $ first (map (fmap (c+))) $ splitAt (matches x) (tail y)

matches :: Card -> Int
matches (a,b) = length $ filter id $ (==) <$> a <*> b
