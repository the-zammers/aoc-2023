module Day05 where

import Paths_aoc2023 (getDataFileName)
import Data.List.Split (splitOn, chunksOf)
import Data.Ix (inRange)
import Data.Tuple (swap)

type Range = (Integer, Integer)

day05 :: IO ()
day05 = do
  contents <- init . lines <$> (getDataFileName "day05.txt" >>= readFile)
  print $ solveA (getSeedsA contents) (format contents)
  print $ solveB (getSeedsB contents) (format contents)

getSeedsA :: [String] -> [Integer]
getSeedsA x = map read $ drop 1 $ words $ head x

getSeedsB :: [String] -> [Range]
getSeedsB x = map (\[a,b]->(a,a+b-1)) $ chunksOf 2 $ getSeedsA x

format :: [String] -> [[(Range, Range)]]
format x = map (map ((\[a,b,c]->((a,a+c-1),(b,b+c-1))) . map read . words) . tail) blocked
  where blocked = splitOn [""] $ drop 2 x

forwardMap, reverseMap :: [[(Range, Range)]] -> Integer -> Integer
forwardMap = foldr1 (.) . reverse . map  functify
reverseMap = foldr1 (.) .           map (functify . map swap)

functify :: [(Range, Range)] -> (Integer -> Integer)
functify xs a = foldr (\(dest,src) -> if inRange src a then const (a - (fst src) + (fst dest)) else id) a xs 

solveA :: [Integer] -> [[(Range, Range)]] -> Integer
solveA seeds maps = minimum $ forwardMap maps <$> seeds

solveB :: [(Integer, Integer)] -> [[(Range, Range)]] -> Integer
solveB seeds maps = fst $ head $ filter (flip any seeds . flip inRange . snd) $ (\f x -> (x, f x)) (reverseMap maps) <$> [0..]
