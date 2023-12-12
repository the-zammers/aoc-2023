{-# LANGUAGE TupleSections #-}

module Day11 where

import Paths_aoc2023 (getDataFileName)
import Data.List (tails, elemIndices)
import Control.Arrow ((***))
import Data.Ix (Ix, inRange)
import Data.Tuple (swap)

type Galaxy = (Int, Int)

day11 :: IO ()
day11 = do
  contents <- init . lines <$> (getDataFileName "day11.txt" >>= readFile)
  let gals = galaxyList contents
  let bnds = bounds contents
  let (ers, ecs) = empties gals bnds
  print $ solve partA ers ecs gals
  print $ solve partB ers ecs gals

galaxyList :: [[Char]] -> [Galaxy]
galaxyList = concat . go 0
  where go _ [] = []
        go n (r:rs) = map (n ,) (elemIndices '#' r) : go (n+1) rs

empties :: [Galaxy] -> (Int, Int) -> ([Int], [Int])
empties xs = isEmpty galrs *** isEmpty galcs 
  where (galrs, galcs) = unzip xs
        isEmpty set = filter (`notElem` set) . enumFromTo 0

solve :: Int -> [Int] -> [Int] -> [Galaxy] -> Int
solve part ers ecs gals = sum $ map (uncurry (dist part ers ecs)) $ pairs gals

partA, partB :: Int
partA = 2
partB = 1000000

dist :: Int -> [Int] -> [Int] -> Galaxy -> Galaxy -> Int
dist scale ers ecs (ay,ax) (by,bx) = abs (ax - bx) + abs (ay - by) + extrars + extracs
  where extracs = (*(scale-1)) . length . filter (between (ax, bx)) $ ecs
        extrars = (*(scale-1)) . length . filter (between (ay, by)) $ ers

bounds :: [[a]] -> (Int, Int)
bounds x = (length x-1, length (head x)-1)

pairs :: [a] -> [(a, a)]
pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

between :: (Ix a) => (a, a) -> a -> Bool
between range x = inRange range x || inRange (swap range) x
