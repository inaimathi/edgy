module Direction where

import Util
import SparseRead

import Data.Map (Map)
import qualified Data.Map as Map

----- Scoring-related stuff
scoreGrid :: Grid -> Map Coord Score
scoreGrid g = Map.mapWithKey (\k _ -> scoreCoord g k) $ gridMap g

scoreCoord :: Grid -> Coord -> Score
scoreCoord g (x, y) = Score (lengthI contigH) (lengthI contigV)
                      (lengthI contigSW) (lengthI contigSE)
    where contig xs ys = findContiguous g $ zip xs ys
          contigSW = concat [ contig [x, pred x..] [y, pred y..]
                            , contig [x..] [y..]]
          contigSE = concat [ contig [x..] [y, pred y..]
                            , contig [x, pred x..] [y..] ]
          contigH = concat [ contig [x..] $ repeat y
                           , contig [x,pred x..] $ repeat y]
          contigV = concat [ contig (repeat x) [y..]
                           , contig (repeat x) [y, pred y..]]

----- Region-related stuff
regions :: Grid -> [Map Coord Direction]
regions g = concatMap (islands 7 . Map.fromList) [ordA, ordB, cardA, cardB, allC]
    where score = scoreGrid g
          (ordA, ordB, ordC) = Map.foldWithKey byOrd ([], [], []) score
          (cardA, cardB, allC) = Map.foldWithKey byCard ([], [], ordC) score
          byOrd k v (a, b, c) = case ordinal v of
                                  SW -> ((k,SW):a, b, c)
                                  SE -> (a, (k,SE):b, c)
                                  _ -> (a, b, (k,C):c)
          byCard k v (a, b, c) = case cardinal v of
                                   H -> ((k,H):a, b, c)
                                   V -> (a, (k,V):b, c)
                                   _ -> (a, b, (k,C):c)

----- The main function
main :: IO ()
main = do g <- readSparse "test.txt"
--          let putGrid = putStrLn . showMap (gridWidth g) (gridHeight g)
          mapM_ putBeside . splitEvery 3 . map (showMap (gridWidth g) (gridHeight g)) $ regions g

----- Data declarations
data Score = Score { north :: Integer, east :: Integer
                   , southWest :: Integer
                   , southEast :: Integer } deriving (Eq)

data Direction = H | V | SW | SE | C deriving (Eq)

data DirType = Cardinal | Ordinal deriving (Eq)

instance Show Direction where
    show H = "-"
    show V = "|"
    show SW = "/"
    show SE = "\\"
    show C = "o"

----- Minor utility/low-level stuff
decide :: Integer -> Score -> Direction
decide threshold s@(Score n e sw se)
    | ((n - threshold) > 0 || (e - threshold) > 0) 
      && (abs $ n - e) > threshold
      && (abs $ n - e) > (abs $ sw - se) = cardinal s
    | ((sw - threshold) > 0 || (se - threshold) > 0) && (abs $ sw - se) > threshold = ordinal s
    | otherwise = C

ordinal :: Score -> Direction
ordinal (Score _ _ sw se) = 
    case se `compare` sw of
      EQ -> C
      GT -> SW
      LT -> SE

cardinal :: Score -> Direction
cardinal (Score n e _ _) = 
    case n `compare` e of
      EQ -> C
      GT -> H
      LT -> V

findContiguous :: Grid -> [Coord] -> [Coord]
findContiguous g cs = recur cs []
    where recur [] acc       = reverse acc
          recur (c:rest) acc = case Map.lookup c (gridMap g) of
                                 Nothing -> recur [] acc
                                 Just _ -> recur rest $ c:acc
