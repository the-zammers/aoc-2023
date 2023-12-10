module Day10 where

import Paths_aoc2023 (getDataFileName)
import ZArray -- would be WAY BETTER if I could get Data.Array working
import Data.Maybe (fromJust)

data Tile = NS | WE | NE | NW | SW | SE | X deriving (Eq)

goesN, goesS, goesW, goesE :: Tile -> Bool
goesN = flip elem [NS, NE, NW]
goesS = flip elem [NS, SW, SE]
goesW = flip elem [WE, NW, SW]
goesE = flip elem [WE, NE, SE]

tileList :: [Tile]
tileList = [NS, WE, NE, NW, SW, SE, X]

type Sketch = Array2 Tile

day10 :: IO ()
day10 = do
  contents <- init . lines <$> (getDataFileName "day10b.txt" >>= readFile)
  let formatted = format contents
  let origin = getOrigin formatted
  let cleaned = clean formatted origin
  print $ solveA cleaned origin
  print $ solveB cleaned origin

format :: [[Char]] -> Array2 (Maybe Tile)
format = fmap charToTile . convert2
  where charToTile = flip lookup $ zip "|-LJ7F." tileList

getOrigin :: Array2 (Maybe Tile) -> Coord2
getOrigin = find Nothing

clean :: Array2 (Maybe Tile) -> Coord2 -> Sketch
clean xs o = fmap fromJust $ replaceIndex xs o $ guessTile xs o

guessTile :: Array2 (Maybe Tile) -> Coord2 -> Maybe Tile
guessTile s pos = [toN, toS, toW, toE] `lookup` mapping
  where toN = (==Just True) $ fmap (goesS . fromJust) $ safeIndex s $ stepN pos 
        toS = (==Just True) $ fmap (goesN . fromJust) $ safeIndex s $ stepS pos 
        toW = (==Just True) $ fmap (goesE . fromJust) $ safeIndex s $ stepW pos 
        toE = (==Just True) $ fmap (goesW . fromJust) $ safeIndex s $ stepE pos 
        mapping = zip (map (([goesN, goesS, goesW, goesE] <*>) . pure) tileList) tileList

solveA :: Sketch -> Coord2 -> Int
solveA s pos = (`div` 2) $ length $ getLoop s [pos]

solveB :: Sketch -> Coord2 -> Int
solveB s pos = length $ getInside s loop
  where loop = getLoop s [pos]

getLoop :: Sketch -> [Coord2] -> [Coord2]
getLoop s seen
  | goesN here && stepN pos `notElem` seen = getLoop s (stepN pos:seen)
  | goesS here && stepS pos `notElem` seen = getLoop s (stepS pos:seen)
  | goesW here && stepW pos `notElem` seen = getLoop s (stepW pos:seen)
  | goesE here && stepE pos `notElem` seen = getLoop s (stepE pos:seen)
  | otherwise = seen
  where here = fromJust $ safeIndex s pos
        pos = head seen

getInside :: Sketch -> [Coord2] -> [Coord2]
getInside s loop = go (0,0) False
  where (maxr, maxc) = bounds s
        go (r,c) inside
          | r>=maxr = []
          | c>=maxc = go (r+1, 0) False
          | (r,c) `elem` loop && (fromJust $ safeIndex s (r,c)) `elem` [NS, NW, NE] = go (r,c+1) (not inside)
          | (r,c) `elem` loop || not inside = go (r,c+1) inside
          | otherwise = (r,c) : go (r,c+1) inside

stepN, stepS, stepW, stepE :: Coord2 -> Coord2
stepN (r,c) = (r-1, c  )
stepS (r,c) = (r+1, c  )
stepW (r,c) = (r  , c-1)
stepE (r,c) = (r  , c+1)
