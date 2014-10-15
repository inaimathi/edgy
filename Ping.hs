module Ping where

import Data.Map (Map)
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
look g w = Map.member (collapse w) $ gridMap g

collide :: Grid -> Wave -> Signal
collide g w = case (look g w, look g $ opposite w) of
                (False, False) -> Echo
                (True, True) -> Silence
                _ -> Lost

waves :: Coord -> [Wave]
waves origin = [ Wave origin (0, 1) 1.0
               , Wave origin (1, 0) 1.0
               , Wave origin (1, 1) 0.75
               , Wave origin (-1, 1) 0.75
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
type Coord = (Integer, Integer)

data Grid = Grid { gridWidth :: Integer, gridHeight :: Integer
                 , gridMap :: Map Coord Char} deriving (Eq, Ord, Show)

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

-- -- none :: [Bool] -> Bool
-- -- none = and . map not

iterateM_ :: Monad m => (a -> m a) -> a -> m b
iterateM_ f a = do res <- f a
                   iterateM_ f res

lengthI :: [a] -> Integer
lengthI = toInteger . length

---------- Debugging stuff
showGrid :: Char -> Grid -> String
showGrid bg g = unlines [collectLine y | y <- [1..h]]
    where collectLine y = [ Map.findWithDefault bg (x, y) m | x <- [1..w]]
          w = gridWidth g
          h = gridHeight g
          m = gridMap g

thin :: Grid -> Grid
thin g = g { gridMap = Map.filterWithKey (\k _ -> ping g k) $ gridMap g}

sparsify :: (Char -> Bool) -> String -> Grid
sparsify predicate str = Grid (toInteger . lengthI $ head ls) (toInteger $ lengthI ls) m
    where ls = lines str
          m = foldl addLine Map.empty $ zip [0..] ls
          addLine memo (ix, line) = foldl (addCell ix) memo $ zip [0..] line
          addCell y memo (x, char)
              | predicate char == True = Map.insert (x, y) char memo
              | otherwise = memo

showWaves :: [Wave] -> String
showWaves ws = unlines [line y | y <- [0..10]]
    where line y = [charOf (x, y) | x <- [0..10]]
          cs = map collapse ws ++ map (collapse . opposite) ws
          charOf coord = if coord `elem` cs
                         then 'X'
                         else ' '

-- putNextTo :: String -> String -> IO ()
-- putNextTo strs = 

putTwoUp :: String -> String -> IO ()
putTwoUp strA strB = mapM_ (\(a, b) -> putStrLn $ concat [a, " + ", b])
               $ zip (lines strA) (lines strB)

main :: IO ()
-- main = mapM_ (putStrLn . showWaves) . take 5 . iterate (map propagate) $ waves (5, 5)
main = do str <- readFile "test.txt"
          let sparse = sparsify (/='.') str
              thinned = thin sparse
              sh = showGrid '.'
          putTwoUp (sh sparse) (sh thinned)
