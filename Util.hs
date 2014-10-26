module Util ( iterateM_, putTwoUp
            , lengthI, sizeI, infI, descending
            , putBeside, splitEvery
            , none) where

import Data.List (sortBy, intercalate)
import Data.Map (Map, size)

descending :: Ord a => [a] -> [a]
descending = sortBy (flip compare)

lengthI :: [a] -> Integer
lengthI = toInteger . length

sizeI :: Map k v -> Integer
sizeI = toInteger . size

putTwoUp :: String -> String -> IO ()
putTwoUp strA strB = mapM_ (\(a, b) -> putStrLn $ concat [a, " + ", b])
               $ zip (lines strA) (lines strB)

iterateM_ :: Monad m => (a -> m a) -> a -> m b
iterateM_ f a = do res <- f a
                   iterateM_ f res

none :: [Bool] -> Bool
none = and . map not

infI :: Integer
infI = toInteger . round $ 1/0

putBeside :: [String] -> IO ()
putBeside strs = putLns $ map lines strs
    where putLns lns
              | any (==[]) lns = putStrLn ""
              | otherwise = do putStrLn . intercalate " + " $ map head lns
                               putLns $ map tail lns

splitEvery :: Int -> [a] -> [[a]]
splitEvery n lst = recur lst []
    where recur [] acc = reverse $ acc
          recur lst acc = recur (drop n lst) $ (take n lst):acc
