module ELSD where

import SparseRead
import Model
import Elements

import Data.List (sortBy)
import Data.Function (on)
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map

computeGradient :: Grid Int -> Grid Float
computeGradient m = Map.mapWithKey angle m
    where angle k _ = let [ a, b, c, d ] = region k
                          gx = fromIntegral $ (b+d) - (a+c)
                          gy = fromIntegral $ (c+d) - (a+b)
                          norm = sqrt $ (gx*gx + gy*gy) / 4.0
                      in if norm <= threshold
                         then notdef
                         else atan2 gy gx
          region (x, y) = map (\c -> Map.findWithDefault 255 c m)
                          [(x+x', y+y') | x' <- [0,1], y' <- [0,1]]

regionGrow :: Coord -> Float -> Grid Float -> Set Coord
regionGrow px v grad = recur [px] grad Set.empty
    where neighborsOf m pxs = filter (relevant m) pxs
          relevant m px = case Map.lookup px m of
                            Nothing -> False
                            Just cV  -> (abs $ abs v - abs cV) >= threshold
          recur [] _ acc    = acc
          recur layer m acc = let nextM = foldl (flip Map.delete) m layer
                                  nextLayer = neighborsOf nextM $ allNeighbors layer
                              in recur nextLayer nextM $ foldl (\memo c -> Set.insert c memo) acc layer

fitRectangle :: Set Coord -> Element -- THIS IS SILLY. STOP BEING SO SILLY.
fitRectangle cs = Line a b
    where Box a b = boxOf . fromCoords $ Set.toList cs

elsd :: Grid Int -> [(Element, Set Coord)]
elsd image = filter (\((Line a b), _) -> a /= b) $ Set.toList $ Map.foldlWithKey regionOf Set.empty grad
    where grad = computeGradient image
          regionOf memo k v = Set.insert elem memo
              where r = regionGrow k v grad
                    elem = (fitRectangle r, r)

main:: IO ()
main = do f <- readPgm "test-data/sanitized-medal.pgm"
          let res = elsd f
              biggies = map snd . take 2 $ sortBy (flip compare `on` len . fst) res
          svgWrite "test-data/sanitized-medal.svg" $ map fst res
          putStrLn . show $ length res
          mapM_ (putStrLn . showGrid . setToGrid) biggies
          putStrLn "Done..."

---------- File emission
setToGrid :: Set Coord -> Grid Char
setToGrid s = fromList . map (\c -> (c, 'x')) $ Set.toList s


svgShow :: Element -> String
svgShow (Line (x, y) (x', y')) = concat ["<line x1=", ss x, " y1=", ss y, " x2=", ss x', " y2=", ss y', " stroke-width=\"3\"/>"]
    where ss = show . show . (*10)

svgWrite :: FilePath -> [Element] -> IO ()
svgWrite _ [] = do return ()
svgWrite fname elems = writeFile fname contents
    where contents = concat [ "<svg width=\"11in\" height=\"8.5in\" "
                            , "viewBox=\"", s $ x-10, " ", s $ y-10, " ", s $ x'+10, " ", s $ y'+10, "\" "
                            , "xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\">"
                            , "<g stroke=\"green\">"
                            , concatMap svgShow elems
                            , "</g></svg>" 
                            ]
          ((x, y), (x', y')) = foldl (\(a', b') (Line a b) -> (minC b (minC a a'), maxC a (maxC b b'))) first elems
          first = let (Line a b) = head elems
                  in (a, b)
          s = show . (*10)

---------- Constants
notdef :: Float
notdef = -1024.0

ang_th :: Float
ang_th = 22.5

prec :: Float
prec = ang_th/180.0

precision :: Float
precision = pi * prec

threshold :: Float
threshold = 2.0 / sin precision
