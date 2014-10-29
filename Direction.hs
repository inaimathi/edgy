module Direction ( Score(..), Direction(..)
                 , scoreGrid, scoreMap, scoreCoord
                 , decide, cardinal, ordinal) where

import Util
import SparseRead
import Elements

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

----- Region-related stuff
regions :: Integer -> Map Coord Score -> [Map Coord Direction]
regions threshold score = concatMap (islands threshold) $ concatMap (splitByVal . flip Map.map score) [ordinal]

----- Generating elements
genElements :: [Map Coord Direction] -> [Element]
genElements [] = []
genElements rs = map gen rs
    where gen region = Line minCoord maxCoord
              where dir = snd . head $ Map.toList region
                    xFn (x, y) ((minX, minY), (maxX, maxY)) = 
                        case dir of
                          H -> ((min x minX, y), (max x maxX, y))
                          V -> ((x, min y minY), (x, max y maxY))
                          SE -> ((min x minX, min y minY), (max x maxX, max y maxY))
                          SW -> ((min x minX, max y minY), (max x maxX, min y maxY))
                          C -> ((min x minX, max y minY), (max x maxX, min y maxY))
                    (minCoord, maxCoord) = extremeCoords xFn region


extremeCoords :: (Coord -> (Coord, Coord) -> (Coord, Coord)) -> Map Coord a -> (Coord, Coord)
extremeCoords fn m = Map.foldWithKey (\k _ memo -> fn k memo) (first, first) m
    where first = fst . head $ Map.toList m

----- The main function
main :: IO ()
main = do g <- readSparse "test.txt"
          -- let score = scoreGrid g
          --     showScore = showMap . flip Map.map score
          -- putBeside $ map showScore [cardinal, ordinal, decide 5]
          mapM_ (putStrLn . show) . genElements . regions 7 $ scoreGrid g

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
