module Elements ( Element(..), getElements, align, thinLines) where

import Util
import Model
import Direction

import qualified Data.Maybe as Maybe 
import Data.Function (on)
import Data.List (sortBy)
import qualified Data.Map as Map

data Element = Line Coord Coord deriving (Eq, Ord, Show, Read)

align :: Integer -> [Element] -> [Element]
align threshold es = foldl (\memo pt -> alignY pt memo) xAligned pts
    where xAligned = foldl (\memo pt -> alignX pt memo) es pts 
          pts = concatMap (\(Line a b) -> [a, b]) es
          alignY (_, y) elems = map (pullY y) elems
          pullY y (Line a b) = Line (pullPtY a) (pullPtY b)
                               where pullPtY (x, y') = if (abs $ y - y') < threshold
                                                       then (x, y)
                                                       else (x, y')
          alignX (x, _) elems = map (pullX x) elems
          pullX x (Line a b) = Line (pullPtX a) (pullPtX b)
                               where pullPtX (x', y) = if (abs $ x - x') < threshold
                                                       then (x, y)
                                                       else (x', y)

-- resolveOverlaps :: [Element] -> [Element]
-- -- TODO; should combine/filter overlapping lines.
-- resolveOverlaps es = undefined

---------- Line-based thinning and element generation
thinLines :: Grid Direction -> Maybe Element
thinLines m
    | Map.null m = Nothing
    | otherwise = case dir of
                    H -> Just $ Line (minX, mid minY maxY) (maxX, mid minY maxY)
                    V -> Just $ Line (mid minX maxX, minY) (mid minX maxX, maxY)
                    SE -> Just $ Line (minX, minY) (maxX, maxY)
                    SW -> Just $ Line (maxX, minY) (minX, maxY)
                    C -> Nothing
                  where dir = snd . head $ Map.toList m
                        Box (minX, minY) (maxX, maxY) = boxOf m
                        mid a b = toInteger . round $ a' + ((b' - a') / 2)
                                  where a' = fromIntegral a
                                        b' = fromIntegral b

computeElements :: Integer -> Grid a -> [Element]
computeElements threshold m = recur . byDistance . Maybe.mapMaybe toInternal . map trimFlash . concatMap (islands threshold) . concatMap splitByVal $ getDirections m
    where toInternal region = case thinLines region of
                                Just ln -> Just (region, ln)
                                Nothing -> Nothing                               
          byDistance = sortBy (flip compare `on` (len . snd))
          len (Line a b) = distance a b
          recur [] = []
          recur (r:rest) = (snd r) : (recur . byDistance $ filterOut (fst r) rest)
          filterOut _ [] = []
          filterOut m (r:rest) = let filtered = Map.difference (fst r) m
                                     thinned = thinLines filtered
                                 in case (sizeI filtered > threshold, thinned) of
                                      (True, Just ln) -> (filtered, ln) : (filterOut m rest)
                                      _ -> filterOut m rest

getElements :: Ord a => Integer -> Grid a -> [Element]
getElements threshold g = concatMap (computeElements threshold) . concatMap (islands threshold) $ splitByVal g
