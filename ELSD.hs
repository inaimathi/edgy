module ELSD where

import SparseRead
import Model
import Elements

import Data.Maybe
import qualified Data.Map as Map

notdef = -1024.0
ang_th = 22.5
prec = ang_th/180.0
precision = pi * prec
threshold = 2.0 / sin precision

computeGradient :: Grid Int -> Grid Float
computeGradient m = Map.mapWithKey angle m
    where angle k v = let [ a, b, c, d ] = region k
                          gx = fromIntegral $ (b+d) - (a+c)
                          gy = fromIntegral $ (c+d) - (a+b)
                          norm = sqrt $ (gx*gx + gy*gy) / 4.0
                      in if norm <= threshold
                         then notdef
                         else atan2 gy gx
          region (x, y) = map (\c -> Map.findWithDefault 255 c m)
                          [(x+x', y+y') | x' <- [0,1], y' <- [0,1]]

regionGrow :: Grid Float -> Grid Float
regionGrow m = recur m [unsafeFirst m] empty
    where val c = fromJust $ Map.lookup c m
          inThresh c v = and [(threshold + val c) >= v, v >= (threshold - val c)]
          qualifies grid c = filter (\cand -> and [Map.member cand grid, Map.member c grid, inThresh c $ val cand]) $ vonNeumann c
          recur _ [] acc = acc
          recur m layer acc = let nextGrid = foldl (flip Map.delete) m layer
                                  nextLayer = concatMap (qualifies nextGrid) $ allNeighbors layer
                              in recur nextGrid nextLayer $ foldl (\memo c -> Map.insert c (val c) memo) acc layer

findRegions :: Grid Float -> [Grid Float]
findRegions grid 
    | 0 == size grid = []
    | otherwise      = r : (findRegions $ Map.difference grid r)
                       where r = regionGrow grid

elsd :: Grid Int -> [Element]
elsd image = Map.foldlWithKey regionOf [] grad
    where grad = computeGradient image
          regionOf memo k v = r:memo
              where r = undefined

main = do f <- readPgm "test-data/sanitized-input.pgm"
          putStrLn . show . filter ((>5) . size) . findRegions $ computeGradient f
