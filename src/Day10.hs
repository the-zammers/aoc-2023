module Day10 where

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
type Presketch = Array Coord2 (Maybe Tile)
type Sketch = Array Coord2 Tile
type Loop = Array Coord2 Bool

day10 :: IO ()
day10 = do
  contents <- init . lines <$> (getDataFileName "day10.txt" >>= readFile)
  let formatted = format contents
  let origin = getOrigin formatted
  let cleaned = clean formatted origin
  print $ solveA cleaned origin
  print $ solveB cleaned origin

format :: [[Char]] -> Presketch
format xs = fmap charToTile $ mkArray ((0,0), (maxr-1, maxc-1)) (\(r,c) -> xs!!r!!c)
  where charToTile = flip lookup $ zip "|-LJ7F." tileList
        (maxr, maxc) = (length xs, length $ head xs)

mkArray :: ((Int, Int), (Int, Int)) -> ((Int, Int) -> a) -> Array (Int, Int) a
mkArray bnds f = array bnds [((r,c), f (r,c)) | r <- [rs..re], c <- [cs..ce]]
  where ((rs, cs), (re, ce)) = bnds

getOrigin :: Presketch -> Coord2
getOrigin xs = fromJust $ lookup Nothing $ map (\(i,e) -> (e,i)) $ assocs xs

clean :: Presketch -> Coord2 -> Sketch
clean xs o = fmap fromJust $ xs // [(o, guessTile xs o)]

guessTile :: Presketch -> Coord2 -> Maybe Tile
guessTile s pos = [toN, toS, toW, toE] `lookup` mapping
  where toN = (==Just True) $ goesS <$> s ! stepN pos 
        toS = (==Just True) $ goesN <$> s ! stepS pos 
        toW = (==Just True) $ goesE <$> s ! stepW pos 
        toE = (==Just True) $ goesW <$> s ! stepE pos 
        mapping = zip (map (([goesN, goesS, goesW, goesE] <*>) . pure) tileList) tileList

solveA :: Sketch -> Coord2 -> Int
solveA s pos = (`div` 2) $ length $ filter id $ elems loop
  where loop = getLoop s pos $ mkArray (bounds s) (const False)

solveB :: Sketch -> Coord2 -> Int
solveB s pos = length $ getInside s loop
  where loop = getLoop s pos $ mkArray (bounds s) (const False)

getLoop :: Sketch -> Coord2 -> Loop -> Loop
getLoop s pos loop
  | goesN (s!pos) && not (loop ! stepN pos) = getLoop s (stepN pos) $ loop // [(stepN pos, True)]
  | goesS (s!pos) && not (loop ! stepS pos) = getLoop s (stepS pos) $ loop // [(stepS pos, True)]
  | goesW (s!pos) && not (loop ! stepW pos) = getLoop s (stepW pos) $ loop // [(stepW pos, True)]
  | goesE (s!pos) && not (loop ! stepE pos) = getLoop s (stepE pos) $ loop // [(stepE pos, True)]
  | otherwise = loop

getInside :: Sketch -> Loop -> [Coord2]
getInside s loop = go (0,0) False
  where (maxr, maxc) = snd $ bounds s
        go :: Coord2 -> Bool -> [Coord2]
        go (r,c) inside
          | r>=maxr = []
          | c>=maxc = go (r+1, 0) False
          | loop!(r,c) && (s!(r,c)) `elem` [NS, NW, NE] = go (r,c+1) (not inside)
          | loop!(r,c) || not inside = go (r,c+1) inside
          | otherwise = (r,c) : go (r,c+1) inside

stepN, stepS, stepW, stepE :: Coord2 -> Coord2
stepN (r,c) = (r-1, c  )
stepS (r,c) = (r+1, c  )
stepW (r,c) = (r  , c-1)
stepE (r,c) = (r  , c+1)
