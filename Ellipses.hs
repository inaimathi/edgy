module Ellipses (isEllipse)where

import Util
import Model

pointAt :: (Floating a, RealFrac a) => Coord -> a -> a -> a -> Coord
pointAt (x, y) t rx ry = (x+(roundI $ rx * (cos t)), y+(roundI $ ry * (sin t)))

ellipsePoints :: Coord -> Integer -> Integer -> [Coord]
ellipsePoints center rx ry = [pointAt center t rx' ry' | t <- [0,10..360]]
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

isEllipse :: Grid Char -> Bool
isEllipse g = checkResults res
    where res = map (flip Model.member g) $ ellipsePoints center rx ry
          (Box a@(x, y) b@(x', y')) = boxOf g
          center = midpoint a b
          rx = (x' - x) `div` 2
          ry = (y' - y) `div` 2
