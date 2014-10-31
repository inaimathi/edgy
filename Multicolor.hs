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

main :: IO ()
main = do f <- readSparse "multi.txt"
          putBeside . map showMap . getDirections $ separate 7 f
          let square = getDirections [last $ separate 7 f]
--          putBeside . (map showMap) $ sortBy (compare `on` sizeI) $ concatMap (islands 7) $ concatMap splitByVal square
          mapM_ putBeside . splitEvery 6 . map showMap . map thinRegion . concatMap (islands 7) . concatMap splitByVal . getDirections $ separate 7 f
