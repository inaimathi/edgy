module Direction ( Direction(..), getDirections, trimFlash) where

import Util
import Model

import Data.Function (on)
import Data.List (sortBy)
import qualified Data.Map as Map

----- Scoring-related stuff
getDirections :: Grid a -> [Grid Direction]
getDirections g = [Map.map cardinal scored, Map.map ordinal scored]
    where scored = scoreGrid g

scoreGrid :: Grid a -> Grid Score
scoreGrid m = Map.mapWithKey (\k _ -> scoreCoord m k) m

scoreCoord :: Grid a -> Coord -> Score
scoreCoord m (x, y) = (contigH, contigV, contigSW, contigSE)
    where contig (xs, ys) = findContiguous m $ zip xs ys
          score = lengthI . concatMap contig
          contigSW = score [ ([x, pred x..], [y, pred y..])
                           , ([x..], [y..])]
          contigSE = score [ ([x..], [y, pred y..])
                           , ([x, pred x..], [y..]) ]
          contigH = score [ ([x..], repeat y)
                          , ([x,pred x..], repeat y)]
          contigV = score [ ((repeat x), [y..])
                          , ((repeat x), [y, pred y..])]

--- Try to do a cellular automaton-based thing (0 and 1 neighbors die, everyone else lives)
trimFlash :: Grid Direction -> Grid Direction
trimFlash m = case dir of 
                H -> diff (w `div` 2) [(rw y) | y <- [minY .. maxY]] 
                V -> diff (h `div` 2) [(cl x) | x <- [minX .. maxX]]
                _ -> m
    where diff min lns = differences m . shortest min $ map Map.fromList lns
          shortest n lns = filter ((n>) . sizeI) lns
          dir = snd . head $ Map.toList m
          w = maxX - minX
          h = maxY - minY
          Box (minX, minY) (maxX, maxY) = boxOf m
          rw y = looks $ zip [minX..maxX] $ repeat y
          cl x = looks $ zip (repeat x) [minY..maxY]
          -- Just friggin make these part of Grid
          looks [] = []
          looks (c:rest) = case Map.lookup c m of
                             Just v  -> (c, v) : (looks rest)
                             Nothing -> looks rest

----- Score
type Score = (Integer, Integer, Integer, Integer)

----- Directions
data Direction = H | V | SW | SE | C deriving (Eq, Ord)

ordinal :: Score -> Direction
ordinal (_, _, sw, se) = 
    case se `compare` sw of
      EQ -> C
      GT -> SW
      LT -> SE

cardinal :: Score -> Direction
cardinal (n, e, _, _) = 
    case n `compare` e of
      EQ -> C
      GT -> H
      LT -> V

instance Show Direction where
    show H = "-"
    show V = "|"
    show SW = "/"
    show SE = "\\"
    show C = "o"
