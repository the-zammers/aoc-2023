module Day05 where

import Paths_aoc2023 (getDataFileName)
import Data.List.Split (splitOn, chunksOf)
--import qualified Data.Finite as F
import Data.Foldable (asum)


day05 :: IO ()
day05 = do
  contents <- init . lines <$> (getDataFileName "day05.txt" >>= readFile)
  --putStrLn $ unlines contents
  print $ solveA (getSeedsA contents) (formatA contents)
  print $ solveB (getSeedsB contents) (formatA contents)

getSeedsA :: [String] -> [Integer]
getSeedsA x = map read $ drop 1 $ words $ head x

getSeedsB :: [String] -> [(Integer, Integer)]
getSeedsB x = map (\[a,b]->(a,b)) $ chunksOf 2 $ getSeedsA x

formatA :: [String] -> [[(Integer, Integer, Integer)]]
formatA x = extractedMaps
  where blocked = splitOn [""] $ drop 2 x
        extractedMaps = map (map ((\[a,b,c]->(a,b,c)) . map read . words) . tail) blocked

myMap :: [(Integer, Integer, Integer)] -> (Integer -> Integer)
myMap xs a = maybe a id (asum $ map (($ a) . myMap') xs)

myMap' :: (Integer, Integer, Integer) -> (Integer -> Maybe Integer)
myMap' (dest, src, len) a | src<=a && a<=src+len-1 = Just $ a - src + dest
myMap' _ _ = Nothing

reverseMap :: [(Integer, Integer, Integer)] -> (Integer -> Integer)
reverseMap xs a = maybe a id (asum $ map (($ a) . reverseMap') xs)

reverseMap' :: (Integer, Integer, Integer) -> (Integer -> Maybe Integer)
reverseMap' (src, dest, len) a | src<=a && a<=src+len-1 = Just $ a - src + dest
reverseMap' _ _ = Nothing

solveA :: [Integer] -> [[(Integer, Integer, Integer)]] -> Integer
solveA seeds maps = minimum $ foldr (<$>) seeds (reverse $ map myMap maps)

solveB :: [(Integer, Integer)] -> [[(Integer, Integer, Integer)]] -> Integer
solveB seeds maps = solveA [solveA [head $ filter (isIn seeds) $ foldr (<$>) [0..] (reverse $ map reverseMap maps)] maps] maps

isIn :: [(Integer, Integer)] -> Integer -> Bool
isIn [] _ = False
isIn ((a,b):rest) x
  | a<=x && x<=a+b-1 = True
  | otherwise = isIn rest x
