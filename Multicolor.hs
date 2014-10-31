module Multicolor where

import Util
import SparseRead
-- import Elements
import Direction

import Data.List (sortBy)
import Data.Function (on)
import Data.Map (Map)
import qualified Data.Map as Map

separate :: Integer -> Grid -> [Map Coord Char]
separate threshold m = concatMap (islands threshold) . splitByVal $ gridMap m

getDirections :: [Map Coord a] -> [Map Coord Direction]
getDirections ms = concatMap (direct [cardinal, ordinal]) ms 
    where direct fns m = let scored = scoreMap m
                         in map (\fn -> Map.map fn scored) fns

getLines :: Map Coord a -> [Map Coord Direction]
getLines m = recur . byThinnedLen . map toInternal . concatMap (islands 7) . concatMap splitByVal $ getDirections [m]
    where toInternal region = (region, thinRegion region)
          byThinnedLen = sortBy (flip compare `on` (Map.size . snd))
          recur [] = []
          recur (r:rest) = (snd r) : (recur $ filterOut (fst r) rest)
          filterOut m [] = []
          filterOut m (r:rest) = let filtered = Map.difference (fst r) m
                                 in if Map.size filtered > 7 
                                    then (filtered, snd r) : (filterOut m rest)
                                    else filterOut m rest

main :: IO ()
main = do f <- readSparse "multi.txt"
          putBeside . map showMap . getDirections $ separate 7 f
          let thinnedLines = concatMap getLines $ separate 7 f
          putBeside $ map showMap thinnedLines
          putStrLn . showMap $ Map.unions thinnedLines
