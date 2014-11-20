module Direction ( Direction(..), getDirections, getDecision) where

import Util
import Model

import Data.List (maximumBy)
import Data.Function (on)
import qualified Data.Map as Map

----- Scoring-related stuff
getDirections :: Grid a -> [Grid Direction]
getDirections g = [Map.map cardinal scored, Map.map ordinal scored]
    where scored = scoreGrid g

getDecision :: Grid a -> Grid Direction
getDecision g = Map.map direction $ scoreGrid g

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

----- Score
type Score = (Integer, Integer, Integer, Integer)

----- Directions
data Direction = H | V | SW | SE | C deriving (Eq, Ord)

-- really, this should check if they're all similar enough to each other first, and declare the score C instead
direction :: Score -> Direction
-- This did something interesting for segmentation. Take a look at using it maybe?
-- direction (n, e, sw, se) = fst . maximumBy (compare `on` snd) $ [(V, n), (H, e), (SW, sw), (SE, se)]
direction (n, e, sw, se) = fst . maximumBy (compare `on` snd) $ [(V, e-n), (H, n-e), (SW, se-sw), (SE, sw-se)]

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
