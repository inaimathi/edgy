module Direction where

import Util
import SparseRead

import Data.Map (Map)
import qualified Data.Map as Map

----- Scoring-related stuff
scoreGrid :: Grid -> Map Coord Score
scoreGrid g = Map.mapWithKey (\k _ -> scoreCoord g k) $ gridMap g

scoreCoord :: Grid -> Coord -> Score
scoreCoord g (x, y) = Score contigH contigV contigSW contigSE
    where contig (xs, ys) = findContiguous g $ zip xs ys
          score = lengthI . concatMap contig
          contigSW = score [ ([x, pred x..], [y, pred y..])
                           , ([x..], [y..])]
          contigSE = score [ ([x..], [y, pred y..])
                           , ([x, pred x..], [y..]) ]
          contigH = score [ ([x..], repeat y)
                          , ([x,pred x..], repeat y)]
          contigV = score [ ((repeat x), [y..])
                          , ((repeat x), [y, pred y..])]

----- Region-related stuff
regions :: Integer -> Map Coord Score -> [Map Coord Direction]
regions threshold score = concatMap (islands threshold) $ concatMap (splitByVal . flip Map.map score) [ordinal, cardinal]

splitByVal :: Ord a => Map Coord a -> [Map Coord a]
splitByVal m = map (\(k, v) -> Map.fromList $ zip v $ repeat k) $ splits
    where splits = Map.toList $ Map.foldWithKey split Map.empty m 
          split k v memo = Map.alter (ins k) v memo
          ins new (Just v) =  Just $ new:v
          ins new Nothing = Just [new]


----- The main function
main :: IO ()
main = do g <- readSparse "test.txt"
          let -- showGrid = showMap (gridWidth g) (gridHeight g)
              -- score = scoreGrid g
              -- showScore = showGrid . flip Map.map score
--          putBeside $ map showScore [cardinal, ordinal, decide 5]
          mapM_ putBeside . splitEvery 3 . map (showMap (gridWidth g) (gridHeight g)) . regions 7 $ scoreGrid g

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

findContiguous :: Grid -> [Coord] -> [Coord]
findContiguous g cs = recur cs []
    where recur [] acc       = reverse acc
          recur (c:rest) acc = case Map.lookup c (gridMap g) of
                                 Nothing -> recur [] acc
                                 Just _ -> recur rest $ c:acc
