module SparseRead (readSparse, readPgm, readPpm) where

import Util
import Model

import Data.Char
import qualified Data.Map as Map
import System.IO
import System.FilePath (takeExtension)

sparsify :: (Char -> Bool) -> String -> Grid Char
sparsify predicate str = foldl addLine empty $ zip [0..] ls
    where ls = lines str
          addLine memo (ix, line) = foldl (addCell ix) memo $ zip [0..] line
          addCell y memo (x, char)
              | predicate char == True = Map.insert (x, y) char memo
              | otherwise = memo

readSparse :: FilePath -> IO (Grid Char)
readSparse fname = case takeExtension fname of
                     ".txt" -> fmap (sparsify (/='.')) $ readFile fname
                     ".pgm" -> fmap (Map.map (const 'x')) $ readPgm fname
                     ".ppm" -> fmap (Map.map (\(r, _, b) -> if r > b then 'x' else 'o')) $ readPpm fname
                     _ -> return empty

getNonComment :: Handle -> IO String
getNonComment h = do ln <- hGetLine h
                     if head ln == '#'
                     then getNonComment h
                     else return ln

getHeaders :: Handle -> IO (String, (Int, Int), Int)
getHeaders h = do ln1 <- getNonComment h
                  ln2 <- getNonComment h
                  ln3 <- getNonComment h
                  let [w, h] = map read $ words ln2
                  return $ (ln1, (w, h), read ln3)

readPgm :: FilePath -> IO (Grid Int)
readPgm fname = do h <- openBinaryFile fname ReadMode
                   (_, (width, _), _) <- getHeaders h
                   img <- hGetContents h
                   let len = width
                       lns = zip [0..] . map (zip [0..]) . splitEvery len $ map ord img
                       ln (y, l) = map (\(x, v) -> ((x, y), v)) $ filter ((190>) . snd) l
                   return . Map.fromList $ concatMap ln lns

readPpm :: FilePath -> IO (Grid (Int, Int, Int))
readPpm fname = do h <- openBinaryFile fname ReadMode
                   (_, (width, _), _) <- getHeaders h
                   img <- hGetContents h
                   let len = width * 3
                       lns = zip [0..] . map (zip [0..] . splitEvery 3) . splitEvery len $ map ord img
                       ln (y, l) = map (\(x, v) -> ((x, y), tup v)) $ filter (any (190>) . snd) l
                       tup [r, g, b] = (r, g, b)
                       tup _ = (0, 0, 0)
                   return . Map.fromList $ concatMap ln lns
