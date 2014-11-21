module Ellipses where

import Util
import Model
import SparseRead

pointAt :: (Floating a, RealFrac a) => Coord -> a -> a -> a -> Coord
pointAt (x, y) t rx ry = (x+(roundI $ rx * (cos t)), y+(roundI $ ry * (sin t)))

ellipsePoints :: Coord -> Integer -> Integer -> [Coord]
ellipsePoints center rx ry = [pointAt center t rx' ry' | t <- [0,10..360]]
    where rx' = fromIntegral rx
          ry' = fromIntegral ry

fitEllipse :: Grid Char -> Grid Char
fitEllipse g = Model.insert center '8' $ foldl markPt g $ ellipsePoints center w h
    where testPoints = ellipsePoints center w h
          markPt memo (x, y) = Model.insert (x, y) 'O' memo
          (Box a@(x, y) b@(x', y')) = boxOf g
          center@(cx, cy) = midpoint a b
          w = (x' - x) `div` 2
          h = (y' - y) `div` 2

centerOf g = midpoint a b
    where (Box a b) = boxOf g

main = do f <- readSparse "test-data/sanitized-medal.pgm"
          let is = islands f
          mapM_ (putStrLn . show . (\i -> (boxOf i, centerOf i))) is
          mapM_ (putStrLn . showCharGrid . fitEllipse) is
