module Elements ( Element, getElements
                , fbShow, fbWrite
                , svgWrite
                , thinLines) where

import Util
import Model
import SparseRead
import Direction

import System.FilePath (replaceExtension)
import qualified Data.Maybe as Maybe 
import Data.Function (on)
import Data.List (sortBy)
import qualified Data.Map as Map

data Element = Line Coord Coord deriving (Eq, Ord, Show, Read)

align :: [Element] -> [Element]
align es = foldl (\memo pt -> alignY pt memo) xAligned pts
    where xAligned = foldl (\memo pt -> alignX pt memo) es pts 
          pts = concatMap (\(Line a b) -> [a, b]) es
          alignY (_, y) elems = map (pullY y) elems
          pullY y (Line a b) = Line (pullPtY a) (pullPtY b)
                               where pullPtY (x, y') = if (abs $ y - y') < 3
                                                       then (x, y)
                                                       else (x, y')
          alignX (x, _) elems = map (pullX x) elems
          pullX x (Line a b) = Line (pullPtX a) (pullPtX b)
                               where pullPtX (x', y) = if (abs $ x - x') < 3
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
                    H -> let longest = findLongest [ln y | y <- [minY..maxY]]
                             Box (minX', _) (maxX', _) = boxOf longest
                         in Just $ Line (minX', mid minY maxY) (maxX', mid minY maxY)
                    V -> let longest = findLongest [col x | x <- [minX..maxX]]
                             Box (_, minY') (_, maxY') = boxOf longest
                         in Just $ Line (mid minX maxX, minY') (mid minX maxX, maxY')
                    SE -> Just $ Line (minX, minY) (maxX, maxY)
                    SW -> Just $ Line (maxX, minY) (minX, maxY)
                    C -> Nothing
                  where dir = snd . head $ Map.toList m
                        Box (minX, minY) (maxX, maxY) = boxOf m
                        mid a b = toInteger . round $ a' + ((b' - a') / 2)
                                  where a' = fromIntegral a
                                        b' = fromIntegral b
                        byLen = sortBy (flip compare `on` length)
                        findLongest cs = let longest = head $ byLen $ map (findContiguous m) cs
                                         in Map.fromList $ zip longest $ repeat dir
                        ln y = [(x, y) | x <- [minX..maxX]]
                        col x = [(x, y) | y <- [minY..maxY]]

computeElements :: Integer -> Grid a -> [Element]
computeElements threshold m = recur . byDistance . Maybe.mapMaybe toInternal . concatMap (islands threshold) . concatMap splitByVal $ getDirections m
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

---------- File emission
svgShow :: Element -> String
svgShow (Line (x, y) (x', y')) = concat ["<line x1=", ss x, " y1=", ss y, " x2=", ss x', " y2=", ss y', " stroke-width=\"2\"/>"]
    where ss = show . show . (*10)

svgWrite :: FilePath -> [Element] -> IO ()
svgWrite fname elems = writeFile fname contents
    where contents = concat [ "<svg width=\"12cm\" height=\"10cm\" viewBox=\"0 0 1200 1000\" xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\">"
                            , "<g stroke=\"green\">"
                            , concatMap svgShow elems
                            , "</g></svg>" ]

fbShow :: String -> Element -> String
fbShow factId (Line (x, y) (x', y')) = 
    concat [ "(", factId, " :LINE-SEGMENT NIL)\n"
           , "(", factId, " :START (", show x, " ", show y, "))\n"
           , "(", factId, " :END (", show x', " ", show y', "))\n"]

fbWrite :: FilePath -> [Element] -> IO ()
fbWrite fname elems = writeFile fname . concat $ map (uncurry fbShow) pairs
    where pairs = zip (map ((":FACT"++) . show) [1..]) elems

---------- Main and related utility
processFile :: FilePath -> IO ()
processFile fname = do f <- readSparse fname
                       putStrLn $ "\n=>" ++ fname
                       let elems = align $ getElements 7 f
                       svgWrite (replaceExtension fname "svg") elems
                       mapM_ (putStrLn . show) elems

main :: IO ()
main = mapM_ processFile [ "test-data/single-color.txt"
                         , "test-data/multi-color.txt"
                         , "test-data/pentagon.txt"
                         , "test-data/pentagon-multiline-to-square.txt"]
