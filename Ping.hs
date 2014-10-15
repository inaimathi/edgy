module Ping where

import Data.Map hiding (foldl, fromList, toList, map)

type Coord = (Integer, Integer)
type Pair = (Coord, Coord)
type Ping = [Pair]
data Grid = Grid { gridWidth :: Integer, gridHeight :: Integer, gridMap :: Map (Integer, Integer) Char} deriving (Eq, Ord, Show)
data Bump = Solid | Glancing | Unsure deriving (Eq)

add :: Coord -> Coord -> Coord
add (a, b) (c, d) = (a + c, b + d)

none :: [Bool] -> Bool
none = and . map not

lengthI :: [a] -> Integer
lengthI = toInteger . length

ping :: Coord -> Integer -> Ping
ping coord n = map (\(a, b) -> (add coord a, add coord b)) 
               [ ((0, n), (0, -n)), ((d, d), (-d, -d))
               , ((d, -d), (-d, d)), ((n, 0), (-n, 0))]
    where d = ceiling $ 3 * (fromIntegral n / 4)

bumpedPair :: Grid -> (Coord, Coord) -> Bump
bumpedPair g (a, b) = case (look a, look b) of
                        (Nothing, Nothing) -> Solid
                        (Just _, Just _) -> Unsure
                        _ -> Glancing
    where look = flip Data.Map.lookup (gridMap g)

bumpedPing :: Grid -> Ping -> Bump
bumpedPing _ [] = Unsure
bumpedPing g (pair:rest)
    | bumpedPair g pair == Solid = Solid
    | bumpedPair g pair == Glancing = Glancing
    | otherwise = bumpedPing g rest

isCentral :: Grid -> Integer -> Coord -> Bool
isCentral g limit pt = recur 1
    where recur n = if n > limit 
                    then False
                    else case bumpedPing g $ ping pt n of
                           Solid -> True
                           Glancing -> False
                           _ -> recur $ succ n

-- So... if any two pairwise coordinates encounter a state change within some tolerance of each other, we've got a keeper point
-- Otherwise (if one or more shell coordinates hit a state change, but none of their complements do within the set tolerance), we can safely remove it

---------- Debugging stuff
showGrid :: Char -> Grid -> String
showGrid background g = unlines [collectLine y | y <- [1..h]]
    where collectLine y = [ findWithDefault background (x, y) m | x <- [1..w]]
          w = gridWidth g
          h = gridHeight g
          m = gridMap g

thin :: Grid -> Grid
thin g = g { gridMap = Data.Map.filterWithKey (\k _ -> isCentral g 5 k) $ gridMap g}

sparsify :: (Char -> Bool) -> String -> Grid
sparsify predicate str = Grid (toInteger . lengthI $ head ls) (toInteger $ lengthI ls) m
    where ls = lines str
          m = foldl addLine empty $ zip [0..] ls
          addLine memo (ix, line) = foldl (addCell ix) memo $ zip [0..] line
          addCell y memo (x, char)
              | predicate char == True = insert (x, y) char memo
              | otherwise = memo

showPing :: (Integer, Integer, Integer, Integer) -> Ping -> String
showPing (minX, minY, maxX, maxY) coords = unlines [line y | y <- [minY..maxY]]
    where line y = [charOf (x, y) | x <- [minX..maxX]]
          cs = (\(as, bs) -> concat [as, bs]) $ unzip coords
          charOf coord = if coord `elem` cs
                         then 'X'
                         else ' '

main :: IO ()
-- main = mapM_ (putStrLn . showPing (-20, -20, 20, 20) . ping (5, 5)) [1..10]
main = do str <- readFile "test.txt"
          putStrLn $ showGrid '.' $ thin $ sparsify (/='.') str
