module SparseRead ( Grid(..), Coord
                  , readSparse, sparsify
                  , member, mooreNeighbors
                  , showGrid, showMap) where

import Util

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

showMap :: Show b => Integer -> Integer -> Map Coord b -> String
showMap width height m = unlines [collectLine y | y <- [1..height]]
    where collectLine y = concat [ case Map.lookup (x, y) m of
                                     Nothing -> " "
                                     Just a -> show a | x <- [1..width]]

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

mooreNeighbors :: Coord -> [Coord]
mooreNeighbors (x, y) = [(x+x', y+y') | x' <- [-1..1], y' <- [-1..1], (x', y') /= (0,0)]
