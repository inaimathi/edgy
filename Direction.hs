module Direction ( Score(..), Direction(..)
                 , scoreGrid, scoreMap, scoreCoord, thinRegion
                 , decide, cardinal, ordinal) where

import Util
import SparseRead
-- import Elements

import Data.List (sortBy)
import Data.Function (on)
import Data.Map (Map)
import qualified Data.Map as Map

----- Scoring-related stuff
scoreGrid :: Grid -> Map Coord Score
scoreGrid g = scoreMap $ gridMap g

scoreMap :: Map Coord a -> Map Coord Score
scoreMap m = Map.mapWithKey (\k _ -> scoreCoord m k) m

scoreCoord :: Map Coord a -> Coord -> Score
scoreCoord m (x, y) = Score contigH contigV contigSW contigSE
    where contig (xs, ys) = findContiguous m $ zip xs ys
          score = lengthI . concatMap contig
          contigSW = score [ ([x, pred x..], [y, pred y..])
                           , ([x..], [y..])]
          contigSE = score [ ([x..], [y, pred y..])
                           , ([x, pred x..], [y..]) ]
          contigH = score [ ([x..], repeat y)
                          , ([x,pred x..], repeat y)]
          contigV = score [ ((repeat x), [y..])
                          , ((repeat x), [y, pred y..])]

----- The main function
main :: IO ()
main = do g <- readSparse "test.txt"
          let score = scoreGrid g
              showScore = showMap . flip Map.map score
          putBeside $ map showScore [cardinal, ordinal, decide 5]

----- Data declarations
data Score = Score { north :: Integer, east :: Integer
                   , southWest :: Integer
                   , southEast :: Integer } deriving (Eq, Ord)

data Direction = H | V | SW | SE | C deriving (Eq, Ord)

data DirType = Cardinal | Ordinal deriving (Eq, Ord)

instance Show Direction where
    show H = "-"
    show V = "|"
    show SW = "/"
    show SE = "\\"
    show C = "o"

thinRegion :: Map Coord Direction -> Map Coord Direction
thinRegion m
    | Map.null m = m
    | otherwise = case snd . head $ Map.toList m of
                    H -> findLongest H [ln y | y <- [minY..maxY]]
                    V -> findLongest V [col x | x <- [minX..maxX]]
                    SE -> findLongest SE [dr x | x <- [minX..maxX]]
                    SW -> findLongest SW [dl x | x <- [minX..maxX]]
                    C -> Map.empty
                  where Box (minX, minY) (maxX, maxY) = boxOf m
                        findLongest dir cs = let longest = head $ byLen $ map (findContiguous m) cs
                                             in Map.fromList $ zip longest $ repeat dir
                        byLen = sortBy (flip compare `on` length)
                        ln y = [(x, y) | x <- [minX..maxX]]
                        col x = [(x, y) | y <- [minY..maxY]]
                        dr x = zip [x..maxX] [minY..maxY]
                        dl x = zip [x..maxX] [maxY, pred maxY..minY]

----- Minor utility/low-level stuff
decide :: Integer -> Score -> Direction
decide threshold s@(Score n e sw se)
    | ((n - threshold) > 0 || (e - threshold) > 0) 
      && (abs $ n - e) > threshold = cardinal s
    | ((sw - threshold) > 0 || (se - threshold) > 0) 
      && (abs $ sw - se) > threshold = ordinal s
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
