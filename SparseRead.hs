module SparseRead ( Grid(..), BoundingBox(..), Coord
                  , readSparse, sparsify
                  , member, allNeighbors, islands, splitByVal, findContiguous, boxOf, distance
                  , showGrid, showMap) where

import Util

import Data.Maybe (fromJust)
import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as Map

type Coord = (Integer, Integer)
data Grid = Grid { gridWidth :: Integer, gridHeight :: Integer
                 , gridMap :: Map Coord Char} deriving (Eq, Ord, Show)

sparsify :: (Char -> Bool) -> String -> Grid
sparsify predicate str = Grid (toInteger . lengthI $ head ls) (toInteger $ lengthI ls) m
    where ls = lines str
          m = foldl addLine Map.empty $ zip [0..] ls
          addLine memo (ix, line) = foldl (addCell ix) memo $ zip [0..] line
          addCell y memo (x, char)
              | predicate char == True = Map.insert (x, y) char memo
              | otherwise = memo

showMap :: Show a => Map Coord a -> String
showMap m = unlines [collectLine y | y <- [minY..maxY]]
    where collectLine y = concat [ case Map.lookup (x, y) m of
                                     Nothing -> " "
                                     Just a -> show a | x <- [minX..maxX]]
          (Box (minX, minY) (maxX, maxY)) = boxOf m

showGrid :: Char -> Grid -> String
showGrid bg g = unlines [collectLine y | y <- [1..h]]
    where collectLine y = [ Map.findWithDefault bg (x, y) m | x <- [1..w]]
          w = gridWidth g
          h = gridHeight g
          m = gridMap g

readSparse :: FilePath -> IO Grid
readSparse = fmap (sparsify (/='.')) . readFile

member :: Grid -> Coord -> Bool
member g c = Map.member c $ gridMap g

-- mooreNeighbors :: Coord -> [Coord]
-- mooreNeighbors (x, y) = [(x+x', y+y') | x' <- [-1..1], y' <- [-1..1], (x', y') /= (0,0)]

vonNeumannNeighbors :: Coord -> [Coord]
vonNeumannNeighbors (x, y) = [(x+x', y+y') | x' <- [-1..1], y' <- [-1..1], abs x' /= abs y']

allNeighbors :: [Coord] -> [Coord]
allNeighbors = nub . concatMap vonNeumannNeighbors

----- Grid Utility
-- A bunch of functions that act on Map Coord a.
-- They make sense here, because they'll need to change if
-- we ever change the representation of a sparse image,
-- and they MAY change if we change the representation of Coord

data BoundingBox = Box { boxTopLeft :: Coord, boxBottomRight :: Coord } deriving (Eq, Ord, Show, Read)

minC :: Coord -> Coord -> Coord
minC (x, y) (x', y') = (min x x', min y y')

maxC :: Coord -> Coord -> Coord
maxC (x, y) (x', y') = (max x x', max y y')

distance :: Coord -> Coord -> Integer
distance (x, y) (x', y') = toInteger $ round . sqrt $ ((maxX - minX) ** 2) + ((maxY - minY) ** 2)
    where [minX, maxX, minY, maxY] = map fromIntegral [min x x', max x x', min y y', max y y']

boxOf :: Map Coord a -> BoundingBox
boxOf m = if Map.null m
          then Box (0, 0) (0, 0)
          else Map.foldWithKey (\k _ (Box a b) -> Box (minC k a) (maxC k b)) (Box first first) m
              where first = fst . head $ Map.toList m
          

islands :: Integer -> Map Coord a -> [Map Coord a]
islands threshold grid = recur grid []
    where recur m acc
              | Map.size m == 0 = acc
              | otherwise = let r = nextRegion m
                            in recur (Map.difference m r)
                                   $ if sizeI r >= threshold
                                     then r:acc else acc

nextRegion :: Map Coord a -> Map Coord a
nextRegion m = recur start m Map.empty
    where start = map fst . take 1 $ Map.toList m
          members grid cs = filter (flip Map.member grid) cs
          val c = fromJust $ Map.lookup c m
          recur [] _ acc = acc
          recur layer grid acc = let nextGrid = foldl (flip Map.delete) grid layer
                                     nextLayer = members nextGrid $ allNeighbors layer
                                 in recur nextLayer nextGrid $ foldl (\memo c -> Map.insert c (val c) memo) acc layer

splitByVal :: Ord a => Map Coord a -> [Map Coord a]
splitByVal m = map (\(k, v) -> Map.fromList $ zip v $ repeat k) $ splits
    where splits = Map.toList $ Map.foldWithKey split Map.empty m 
          split k v memo = Map.alter (ins k) v memo
          ins new (Just v) =  Just $ new:v
          ins new Nothing = Just [new]

findContiguous :: Map Coord a -> [Coord] -> [Coord]
findContiguous m cs = recur cs []
    where recur [] acc       = reverse acc
          recur (c:rest) acc = case Map.lookup c m of
                                 Nothing -> recur [] acc
                                 Just _ -> recur rest $ c:acc
