module Day07 where

import Paths_aoc2023 (getDataFileName)
import Data.List (elemIndex, sortBy, sort, maximumBy, group, partition)
import Data.Function (on)
import Control.Arrow ((***), second)
import Data.Maybe (fromJust)

data Card = Joker | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
  deriving (Eq, Ord, Enum)
data Rank = Junk | Pair | TwoPair | Triplet | FullHouse | Quartet | Quintet
  deriving (Eq, Ord)
type Hand = ([Card], Int)

day07 :: IO ()
day07 = do
  contents <- init . lines <$> (getDataFileName "day07.txt" >>= readFile)
  print $ solve $ format partA $ contents
  print $ solve $ format partB $ contents

format :: [Char] -> [String] -> [Hand]
format encoding = map $ (map toCard *** read) . break (==' ')
 where toCard = toEnum . fromJust . flip elemIndex encoding

partA, partB :: [Char]
partA = ".23456789TJQKA"
partB = "J23456789T.QKA"

solve :: [Hand] -> Int
solve = sum . zipWith (*) [1..] . map snd . sortBy (compareHand `on` fst)

compareHand :: [Card] -> [Card] -> Ordering
compareHand a b = compare (getRank a) (getRank b) <> compare a b

getRank :: [Card] -> Rank
getRank cards
  | unique==5               = Junk
  | unique==4               = Pair
  | unique==3 && biggest==2 = TwoPair
  | unique==3 && biggest==3 = Triplet
  | unique==2 && biggest==3 = FullHouse
  | unique==2 && biggest==4 = Quartet
  | unique==1               = Quintet
  | otherwise               = Quintet -- all jokers
  where unique  = length $ nonjokers
        biggest = length $ jokers ++ maximumBy (compare `on` length) nonjokers
        (jokers, nonjokers) = second group $ partition (==Joker) $ sort cards
