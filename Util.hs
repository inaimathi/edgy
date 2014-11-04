module Util ( lengthI, sizeI
            , putBeside, splitEvery) where

import Data.List (intercalate)
import Data.Map (Map, size)

lengthI :: [a] -> Integer
lengthI = toInteger . length

sizeI :: Map k v -> Integer
sizeI = toInteger . size

putBeside :: [String] -> IO ()
putBeside strs = putLns $ map padOut ls
    where ls = map lines strs
          maxLen = foldl max 0 $ map length ls
          padOut [] = take maxLen $ repeat ""
          padOut lns = take maxLen $ lns ++ repeat (take (length $ head lns) $ repeat ' ')
          putLns lns
              | any (==[]) lns = putStrLn ""
              | otherwise = do putStrLn . intercalate " + " $ map head lns
                               putLns $ map tail lns

splitEvery :: Int -> [a] -> [[a]]
splitEvery n lst = recur lst
    where recur [] = []
          recur l = take n l : (recur $ drop n l)
