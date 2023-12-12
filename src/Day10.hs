module Day10 where

-- todo: create a new sketch highlighting only the points that are in the loop instead of a list
-- (elem is slow, ! is fast)

import Paths_aoc2023 (getDataFileName)
import Data.Maybe (fromJust)
import Data.Array

data Tile = NS | WE | NE | NW | SW | SE | X deriving (Eq)

goesN, goesS, goesW, goesE :: Tile -> Bool
goesN = flip elem [NS, NE, NW]
goesS = flip elem [NS, SW, SE]
goesW = flip elem [WE, NW, SW]
goesE = flip elem [WE, NE, SE]

tileList :: [Tile]
tileList = [NS, WE, NE, NW, SW, SE, X]

type Coord2 = (Int, Int)
type Sketch = Array Coord2 Tile

day10 :: IO ()
day10 = do
  contents <- init . lines <$> (getDataFileName "day10.txt" >>= readFile)
  let formatted = format contents
  let origin = getOrigin formatted
  let cleaned = clean formatted origin
  print $ solveA cleaned origin
  print $ solveB cleaned origin

format :: [[Char]] -> Array Coord2 (Maybe Tile)
format xs = fmap charToTile $ array ((0,0), (maxr-1, maxc-1)) [((r,c), xs!!r!!c) | r <- [0..maxr-1], c <- [0..maxc-1]]
  where charToTile = flip lookup $ zip "|-LJ7F." tileList
        (maxr, maxc) = (length xs, length $ head xs)

getOrigin :: Array Coord2 (Maybe Tile) -> Coord2
getOrigin xs = fromJust $ lookup Nothing $ map (\(i,e) -> (e,i)) $ assocs xs

clean :: Array Coord2 (Maybe Tile) -> Coord2 -> Sketch
clean xs o = fmap fromJust $ xs // [(o, guessTile xs o)]

guessTile :: Array Coord2 (Maybe Tile) -> Coord2 -> Maybe Tile
guessTile s pos = [toN, toS, toW, toE] `lookup` mapping
  where toN = (==Just True) $ goesS <$> s ! stepN pos 
        toS = (==Just True) $ goesN <$> s ! stepS pos 
        toW = (==Just True) $ goesE <$> s ! stepW pos 
        toE = (==Just True) $ goesW <$> s ! stepE pos 
        mapping = zip (map (([goesN, goesS, goesW, goesE] <*>) . pure) tileList) tileList

safeIndex :: Array (Int, Int) a -> Coord2 -> Maybe a
safeIndex xs (r,c)
  | r>=maxr || r<0 || c>=maxc || c<0 = Nothing
  | otherwise = Just $ xs!(r,c)
  where (maxr, maxc) = snd $ bounds xs

solveA :: Sketch -> Coord2 -> Int
solveA s pos = (`div` 2) $ length loop
  where loop = getLoop s [pos]

solveB :: Sketch -> Coord2 -> Int
solveB s pos = length $ getInside s loop
  where loop = getLoop s [pos]

getLoop :: Sketch -> [Coord2] -> [Coord2]
getLoop s seen
  | goesN (s!pos) && stepN pos `notElem` seen = getLoop s (stepN pos:seen)
  | goesS (s!pos) && stepS pos `notElem` seen = getLoop s (stepS pos:seen)
  | goesW (s!pos) && stepW pos `notElem` seen = getLoop s (stepW pos:seen)
  | goesE (s!pos) && stepE pos `notElem` seen = getLoop s (stepE pos:seen)
  | otherwise = seen
  where pos = head seen

getInside :: Sketch -> [Coord2] -> [Coord2]
getInside s loop = go (0,0) False
  where (maxr, maxc) = snd $ bounds s
        go :: Coord2 -> Bool -> [Coord2]
        go (r,c) inside
          | r>=maxr = []
          | c>=maxc = go (r+1, 0) False
          | (r,c) `elem` loop && (s!(r,c)) `elem` [NS, NW, NE] = go (r,c+1) (not inside)
          | (r,c) `elem` loop || not inside = go (r,c+1) inside
          | otherwise = (r,c) : go (r,c+1) inside

stepN, stepS, stepW, stepE :: Coord2 -> Coord2
stepN (r,c) = (r-1, c  )
stepS (r,c) = (r+1, c  )
stepW (r,c) = (r  , c-1)
stepE (r,c) = (r  , c+1)
