module Elements ( Element(..)
                , fbShow, fbWrite
                , thinLines) where

import Util
import SparseRead
import Direction
import Multicolor

import qualified Data.Maybe as Maybe 
import Data.Function (on)
import Data.List (sortBy)
import Data.Map (Map)
import qualified Data.Map as Map

data Element = Line Coord Coord deriving (Eq, Ord, Show, Read)
class ToSVG a where
    toSVG :: a -> String

instance ToSVG Element where
    toSVG (Line (x, y) (x', y')) = concat ["<line x1=", ss x, " y1=", ss y, " x2=", ss x', " y2=", ss y', " stroke-width=\"2\"/>"]
        where ss = show . show . (*10)

svgWrite :: FilePath -> [Element] -> IO ()
svgWrite fname elems = writeFile fname contents
    where contents = concat [ "<svg width=\"12cm\" height=\"10cm\" viewBox=\"0 0 1200 1000\" xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\">"
                            , "<g stroke=\"green\">"
                            , concatMap toSVG elems
                            , "</g></svg>" ]

fbShow :: String -> Element -> String
fbShow factId (Line (x, y) (x', y')) = 
    concat [ "(", factId, " :LINE-SEGMENT NIL)\n"
           , "(", factId, " :START (", show x, " ", show y, "))\n"
           , "(", factId, " :END (", show x', " ", show y', "))\n"]

fbWrite :: FilePath -> [Element] -> IO ()
fbWrite fname elems = writeFile fname . concat $ map (uncurry fbShow) pairs
    where pairs = zip (map ((":FACT"++) . show) [1..]) elems

---------- Line-based thinning
thinLines :: Map Coord Direction -> Maybe Element
thinLines m
    | Map.null m = Nothing
    | otherwise = case snd . head $ Map.toList m of
                    H -> Just $ Line (minX, mid minY maxY) (maxX, mid minY maxY)
                    V -> Just $ Line (mid minX maxX, minY) (mid minX maxX, maxY)
                    SE -> Just $ Line (minX, minY) (maxX, maxY)
                    SW -> Just $ Line (maxX, minY) (minX, maxY)
                    C -> Nothing
                  where Box (minX, minY) (maxX, maxY) = boxOf m
                        mid a b = toInteger . round $ a' + ((b' - a') / 2)
                                  where a' = fromIntegral a
                                        b' = fromIntegral b

---------- Line-based generation
computeElems :: Map Coord a -> [Element]
computeElems m = recur . byDistance . Maybe.mapMaybe toInternal . concatMap (islands 7) . concatMap splitByVal $ getDirections [m]
    where toInternal region = case thinLines region of
                                Just ln -> Just (region, ln)
                                Nothing -> Nothing                               
          byDistance = sortBy (flip compare `on` (len . snd))
          len (Line a b) = distance a b
          recur [] = []
          recur (r:rest) = (snd r) : (recur . byDistance $ filterOut (fst r) rest)
          filterOut m [] = []
          filterOut m (r:rest) = let filtered = Map.difference (fst r) m
                                     thinned = thinLines filtered
                                 in case (Map.size filtered > 7, thinned) of
                                      (True, Just ln) -> (filtered, ln) : (filterOut m rest)
                                      _ -> filterOut m rest

computeFromGrid :: Integer -> Grid -> [Element]
computeFromGrid threshold g = concatMap computeElems . concatMap (islands threshold) . splitByVal $ gridMap g

main :: IO ()
main = do f <- readSparse "multi.txt"
          let elems = computeFromGrid 7 f
          putStrLn . show $ map (\(Line a b) -> distance a b) elems
          svgWrite "test.svg" elems
          mapM_ (putStrLn . show) elems
