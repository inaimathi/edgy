module Ping where

import Util
import SparseRead

import qualified Data.Map as Map

collapse :: Wave -> Coord
collapse (Wave (x, y) (x', y') rate) = (x + x'', y + y'')
    where x'' = round $ fromIntegral x' * rate
          y'' = round $ fromIntegral y' * rate

propagate :: Wave -> Wave
propagate (Wave origin (x', y') rate) = Wave origin (inc x', inc y') rate
    where inc 0 = 0
          inc n
              | n > 0 = succ n
              | otherwise = pred n

opposite :: Wave -> Wave
opposite (Wave origin (x', y') rate) = Wave origin (-x', -y') rate

look :: Grid -> Wave -> Bool
look g w = member g $ collapse w

collide :: Grid -> Wave -> Signal
collide g w = case (look g w, look g $ opposite w) of
                (False, False) -> Echo
                (True, True) -> Silence
                _ -> Lost

waves :: Coord -> [Wave]
waves origin = [ Wave origin (0, 1) 1.0
               , Wave origin (1, 0) 1.0
--               , Wave origin (1, 1) 0.75
--               , Wave origin (-1, 1) 0.75
               ]

ping :: Grid -> Coord -> Bool
-- If any wave encounters a state change simultaneously at each end, we've got a keeper.
-- Otherwise, recur, filtering any lost Signals. 
-- If all signals are Lost, the pinging point can be safely removed.
ping g origin = recur $ waves origin
    where recur [] = False
          recur w
              | any ((==Echo) . collide g) w = True
              | otherwise = recur $ next w
          next = filter ((/=Lost) . collide g) . map propagate

---------- Base Types
data Signal = Echo | Silence | Lost deriving (Eq, Show)

data Wave = Wave Coord Coord Float deriving (Eq, Ord, Show)

---------- Minor Utility
-- float :: (Integral a, Num a) => a -> Float
-- float = fromIntegral

-- num :: PairPart -> Integer
-- num (Moving _ n) = n
-- num (Fixed n) = n

-- tuple :: Pair -> (Integer, Integer)
-- tuple (Pair a b) = (num a, num b) 

---------- Debugging stuff
thin :: Grid -> Grid
thin g = g { gridMap = Map.filterWithKey (\k _ -> ping g k) $ gridMap g}

showWaves :: [Wave] -> String
showWaves ws = unlines [line y | y <- [0..10]]
    where line y = [charOf (x, y) | x <- [0..10]]
          cs = map collapse ws ++ map (collapse . opposite) ws
          charOf coord = if coord `elem` cs
                         then 'X'
                         else ' '

main :: IO ()
-- main = mapM_ (putStrLn . showWaves) . take 5 . iterate (map propagate) $ waves (5, 5)
main = do str <- readFile "test.txt"
          let sparse = sparsify (/='.') str
              thinned = thin sparse
              sh = showGrid '.'
          putTwoUp (sh sparse) (sh thinned)
