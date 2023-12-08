module Day08 where

import Paths_aoc2023 (getDataFileName)
import Data.Char (isAlphaNum)
import Data.Maybe (fromJust)
import Control.Arrow (second)

type Node = (String, (String, String))

day08 :: IO ()
day08 = do
  contents <- init . lines <$> (getDataFileName "day08.txt" >>= readFile)
  let instructions = formatInstructions $ contents
  let nodes = formatNodes $ contents
  let starts = formatStarts $ contents
  print $ solve nodes instructions ["AAA"]
  print $ solve nodes instructions starts

formatInstructions :: [String] -> [Char]
formatInstructions = cycle . head

formatNodes :: [String] -> [Node]
formatNodes x = map (second (splitAt 3) . splitAt 3 . filter isAlphaNum) $ drop 2 x

formatStarts :: [String] -> [String]
formatStarts = filter ((=='A') . last) . map fst . formatNodes

solve :: [Node] -> [Char] -> [String] -> Int
solve nodes instructions = foldr1 lcm . map (steps nodes instructions)

steps :: [Node] -> [Char] -> String -> Int
steps nodes lrs loc
  | last loc == 'Z' = 0
  | otherwise = 1 + steps nodes (tail lrs) nextLoc
  where nextLoc = (if head lrs=='L' then fst else snd) $ fromJust $ lookup loc nodes
