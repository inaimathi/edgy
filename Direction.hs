module Direction ( Direction(..), getDirections ) where

import Util
import Model

import qualified Data.Map as Map

----- Scoring-related stuff
getDirections :: Grid a -> [Grid Direction]
getDirections g = [Map.map cardinal scored, Map.map ordinal scored]
    where scored = scoreGrid g

scoreGrid :: Grid a -> Grid Score
scoreGrid m = Map.mapWithKey (\k _ -> scoreCoord m k) m

scoreCoord :: Grid a -> Coord -> Score
scoreCoord m (x, y) = Score contigH contigV contigSW contigSE
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

----- Score
data Score = Score { north :: Integer, east :: Integer
                   , southWest :: Integer, southEast :: Integer } deriving (Eq, Ord)

----- Directions
data Direction = H | V | SW | SE | C deriving (Eq, Ord)

ordinal :: Score -> Direction
ordinal (Score _ _ sw se) = 
    case se `compare` sw of
      EQ -> C
      GT -> SW
      LT -> SE

cardinal :: Score -> Direction
cardinal (Score n e _ _) = 
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
