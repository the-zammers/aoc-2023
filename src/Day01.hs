module Day01 where

import Paths_aoc2023 (getDataFileName)
import Data.Char (isDigit, intToDigit)
import Data.List (isPrefixOf, findIndex)

day01 :: IO ()
day01 = do
  inputLines <- init . lines <$> (getDataFileName "day01.txt" >>= readFile)
  print $ sum $ map (solve . cleanA) $ inputLines
  print $ sum $ map (solve . cleanB) $ inputLines

solve :: [Char] -> Int
solve x = read [head x, last x]

cleanA :: [Char] -> [Char]
cleanA = filter isDigit

cleanB :: [Char] -> [Char]
cleanB = filter isDigit . go
 where go [] = []
       go x = maybe (head x) intToDigit (findIndex (flip isPrefixOf x) digits) : cleanB (tail x)

digits :: [String]
digits = [".", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
