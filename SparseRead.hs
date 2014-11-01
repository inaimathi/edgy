module SparseRead ( readSparse, sparsify ) where

import Model

import qualified Data.Map as Map

sparsify :: (Char -> Bool) -> String -> Grid Char
sparsify predicate str = foldl addLine Map.empty $ zip [0..] ls
    where ls = lines str
          addLine memo (ix, line) = foldl (addCell ix) memo $ zip [0..] line
          addCell y memo (x, char)
              | predicate char == True = Map.insert (x, y) char memo
              | otherwise = memo

readSparse :: FilePath -> IO (Grid Char)
readSparse = fmap (sparsify (/='.')) . readFile
