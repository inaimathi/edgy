module Elements ( Element(..), computeElements, computeEllipse, align) where

import Util
import Model
import Direction

import qualified Data.Maybe as Maybe 
import Data.Function (on)
import Data.List (sortBy)
import qualified Data.Map as Map

data Element = Line Coord Coord 
             | Ellipse Coord Coord deriving (Eq, Ord, Show, Read)

weight :: Element -> Integer
weight (Line a b) = distance a b
weight (Ellipse a b) = distance a b

alignCoord :: Integer -> Coord -> Coord -> Coord
alignCoord threshold (x, y) (x', y') = (check x x', check y y')
    where check a b = if (abs $ a - b) < threshold then a else b

alignElem :: Integer -> Coord -> Element -> Element
alignElem t c (Line a b)    = Line    (alignCoord t c a) (alignCoord t c b)
alignElem t c (Ellipse a b) = Ellipse (alignCoord t c a) (alignCoord t c b)

pointsOf :: Element -> [Coord]
pointsOf (Line a b)    = [a, b]
pointsOf (Ellipse a b) = [a, b]

align :: Integer -> [Element] -> [Element]
align threshold elems = recur es
    where recur (e:rest) = e : (recur $ foldl (\memo pt -> map (alignEl pt) memo) rest $ pointsOf e)
          recur [] = []
          alignEl = alignElem threshold
          es = sortBy (flip compare `on` weight) elems

-- resolveOverlaps :: [Element] -> [Element]
-- -- TODO; should combine/filter overlapping lines.
-- resolveOverlaps es = undefined

---------- Line-based thinning and element generation
thinLine :: Grid Direction -> Maybe Element
thinLine m
    | Map.null m = Nothing
    | otherwise = case dir of
                    H -> Just $ Line (minX, mid minY maxY) (maxX, mid minY maxY)
                    V -> Just $ Line (mid minX maxX, minY) (mid minX maxX, maxY)
                    SE -> Just $ Line (minX, minY) (maxX, maxY)
                    SW -> Just $ Line (maxX, minY) (minX, maxY)
                    C -> Nothing
                  where dir = snd . head $ toList m
                        Box (minX, minY) (maxX, maxY) = boxOf m
                        mid a b = toInteger . round $ a' + ((b' - a') / 2)
                                  where a' = fromIntegral a
                                        b' = fromIntegral b

computeEllipse :: Grid a -> Element
computeEllipse g = Ellipse a b
    where (Box a b) = boxOf g

computeElements :: Integer -> [Grid Direction] -> [Element]
computeElements threshold m = recur . byDistance $ Maybe.mapMaybe toInternal m
    where toInternal region = case thinLine region of
                                Just ln -> Just (region, ln)
                                Nothing -> Nothing                               
          byDistance = sortBy (flip compare `on` (weight . snd))
          recur [] = []
          recur (r:rest) = (snd r) : (recur . byDistance $ filterOut (fst r) rest)
          filterOut _ [] = []
          filterOut m (r:rest) = let filtered = Map.difference (fst r) m
                                     thinned = thinLine filtered
                                 in case (sizeI filtered > threshold, thinned) of
                                      (True, Just ln) -> (filtered, ln) : (filterOut m rest)
                                      _ -> filterOut m rest
