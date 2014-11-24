module ShapeDetection (isEllipse) where

import Util
import Model

import SparseRead

pointAt :: (Floating a, RealFrac a) => Coord -> a -> a -> a -> Coord
pointAt (x, y) t rx ry = (x+(roundI $ rx * (cos t)), y+(roundI $ ry * (sin t)))

ellipsePoints :: Coord -> Integer -> Integer -> [Coord]
ellipsePoints center rx ry = [pointAt center t rx' ry' | t <- [0,20..200]]
    where rx' = fromIntegral rx
          ry' = fromIntegral ry

checkResults :: [Bool] -> Bool
checkResults res = recur res 0
    where recur [] _ = True
          recur (False:False:False:_) _ = False
          recur (False:rest) ct
              | ct >= 8   = False
              | otherwise = recur rest $ succ ct
          recur (_:rest) ct = recur rest ct

potentialEllipse :: Grid a -> [Coord]
potentialEllipse g = ellipsePoints center rx ry
    where (Box a@(x, y) b@(x', y')) = boxOf g
          center = midpoint a b
          rx = (x' - x) `div` 2
          ry = (y' - y) `div` 2

isEllipse :: Grid a -> Bool
isEllipse g = checkResults res
    where res = map (flip Model.member g) $ potentialEllipse g

traceEllipse :: Grid Char -> Grid Char
traceEllipse g = foldl (\memo pt -> Model.insert pt 'M' memo) g $ potentialEllipse g

-- main :: IO ()
-- main = do f <- readSparse "test-data/sanitized-ellipse1.ppm"
--           mapM_ (putStrLn . showCharGrid . traceEllipse) [f]
