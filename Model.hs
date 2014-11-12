module Model ( Coord, minC, maxC, distance, findContiguous, dropLeadingEmpties, allNeighbors, member
             , BoundingBox(..), boxOf
             , Grid, showGrid, showCharGrid, islands, splitByVal) where

import Util

import Data.Maybe (fromJust)
import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as Map
----- Coords
type Coord = (Integer, Integer)

minC :: Coord -> Coord -> Coord
minC (x, y) (x', y') = (min x x', min y y')

maxC :: Coord -> Coord -> Coord
maxC (x, y) (x', y') = (max x x', max y y')

distance :: Coord -> Coord -> Integer
distance (x, y) (x', y') = toInteger $ round . sqrt $ ((maxX - minX) ** 2) + ((maxY - minY) ** 2)
    where [minX, maxX, minY, maxY] = map fromIntegral [min x x', max x x', min y y', max y y']

findContiguous :: Grid a -> [Coord] -> [Coord]
findContiguous m cs = recur cs []
    where recur [] acc       = reverse acc
          recur (c:rest) acc = case Map.lookup c m of
                                 Nothing -> recur [] acc
                                 Just _ -> recur rest $ c:acc

dropLeadingEmpties :: Eq a => Grid a -> [Coord] -> [Coord]
dropLeadingEmpties m cs = dropWhile ((==Nothing) . flip Map.lookup m) cs

member :: Grid a -> Coord -> Bool
member g c = Map.member c g

vonNeumannNeighbors :: Coord -> [Coord]
vonNeumannNeighbors (x, y) = [(x+x', y+y') | x' <- [-1..1], y' <- [-1..1], abs x' /= abs y']

allNeighbors :: [Coord] -> [Coord]
allNeighbors = nub . concatMap vonNeumannNeighbors


----- Bounding Boxes
data BoundingBox = Box { boxTopLeft :: Coord, boxBottomRight :: Coord } deriving (Eq, Ord, Show, Read)

boxOf :: Grid a -> BoundingBox
boxOf m = if Map.null m
          then Box (0, 0) (0, 0)
          else Map.foldWithKey (\k _ (Box a b) -> Box (minC k a) (maxC k b)) (Box first first) m
              where first = fst . head $ Map.toList m

----- Grids
type Grid a = Map Coord a

showCharGrid :: Grid Char -> String
showCharGrid m = unlines [ln y | y <- [minY..maxY]]
    where ln y = concat [ case Map.lookup (x, y) m of
                            Nothing -> " "
                            Just a -> [a] | x <- [minX..maxX]]
          (Box (minX, minY) (maxX, maxY)) = boxOf m

showGrid :: Show a => Grid a -> String
showGrid m = unlines [ln y | y <- [minY..maxY]]
    where ln y = concat [ case Map.lookup (x, y) m of
                            Nothing -> " "
                            Just a -> show a | x <- [minX..maxX]]
          (Box (minX, minY) (maxX, maxY)) = boxOf m

islands :: Integer -> Grid a -> [Grid a]
islands threshold grid = recur grid []
    where recur m acc
              | Map.size m == 0 = acc
              | otherwise = let r = nextRegion m
                            in recur (Map.difference m r)
                                   $ if sizeI r >= threshold
                                     then r:acc else acc

nextRegion :: Grid a -> Grid a
nextRegion m = recur start m Map.empty
    where start = map fst . take 1 $ Map.toList m
          members grid cs = filter (flip Map.member grid) cs
          val c = fromJust $ Map.lookup c m
          recur [] _ acc = acc
          recur layer grid acc = let nextGrid = foldl (flip Map.delete) grid layer
                                     nextLayer = members nextGrid $ allNeighbors layer
                                 in recur nextLayer nextGrid $ foldl (\memo c -> Map.insert c (val c) memo) acc layer

splitByVal :: Ord a => Grid a -> [Grid a]
splitByVal m = map (\(k, v) -> Map.fromList $ zip v $ repeat k) $ splits
    where splits = Map.toList $ Map.foldWithKey split Map.empty m 
          split k v memo = Map.alter (ins k) v memo
          ins new (Just v) =  Just $ new:v
          ins new Nothing = Just [new]
