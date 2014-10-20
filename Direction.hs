module Direction where

import Util
import SparseRead

import Data.List (sortBy)
import Data.Function (on)
import Data.Map (Map)
import qualified Data.Map as Map

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
    show C = "O"

ordinal (Score _ _ sw se) = 
    case se `compare` sw of
      EQ -> C
      GT -> SW
      LT -> SE

cardinal (Score n e _ _) = 
    case n `compare` e of
      EQ -> C
      GT -> H
      LT -> V

findOrdered :: Grid -> [Coord] -> [Coord]
findOrdered g cs = recur cs []
    where recur [] acc       = reverse acc
          recur (c:rest) acc = case Map.lookup c (gridMap g) of
                                 Nothing -> recur [] acc
                                 Just _ -> recur rest $ c:acc

scoreCoord :: Grid -> Coord -> Score
scoreCoord g (x, y) = Score (lengthI contigH) (lengthI contigV)
                      (lengthI contigSW) (lengthI contigSE)
    where contig xs ys = findOrdered g $ zip xs ys
          contigSW = concat [ contig [x, pred x..] [y, pred y..]
                            , contig [x..] [y..]]
          contigSE = concat [ contig [x..] [y, pred y..]
                            , contig [x, pred x..] [y..] ]
          contigH = concat [ contig [x..] $ repeat y
                           , contig [x,pred x..] $ repeat y]
          contigV = concat [ contig (repeat x) [y..]
                           , contig (repeat x) [y, pred y..]]          

scoreGrid :: Grid -> Map Coord Score
scoreGrid g = Map.mapWithKey (\k _ -> scoreCoord g k) $ gridMap g

main :: IO ()
main = do g <- readSparse "test.txt"
          let score = scoreGrid g
              dirMap fn = showMap (gridWidth g) (gridHeight g) $ Map.map fn score
          putTwoUp (dirMap cardinal) (dirMap ordinal)
