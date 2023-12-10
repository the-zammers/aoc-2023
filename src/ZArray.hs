module ZArray where

import Data.Vector ((!), (!?), (//))
import qualified Data.Vector as V

type Coord2 = (Int, Int)
newtype Array2 a = Array2 (V.Vector (V.Vector a)) deriving (Show)

instance Functor Array2 where
  fmap f (Array2 x) = Array2 $ V.map (V.map f) x

safeIndex :: Array2 a -> Coord2 -> Maybe a
safeIndex (Array2 v) (a,b) = v !? a >>= (!? b)

find :: (Eq a) => a -> Array2 a -> Coord2
find x (Array2 xs) = V.head $ do
  (r, line) <- V.zip (V.fromList [0..]) xs
  c <- V.elemIndices x line
  return (r,c)

replaceIndex :: Array2 a -> Coord2 -> a -> Array2 a
replaceIndex (Array2 s) (r,c) p = Array2 $ s // [(r, s!r // [(c, p)])]

convert2 :: [[a]] -> Array2 a
convert2 = Array2 . V.fromList . map V.fromList

bounds :: Array2 a -> (Int, Int)
bounds (Array2 a) = (V.length a, V.length (V.head a))
