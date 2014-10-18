module Util ( iterateM_, putTwoUp
            , none) where

putTwoUp :: String -> String -> IO ()
putTwoUp strA strB = mapM_ (\(a, b) -> putStrLn $ concat [a, " + ", b])
               $ zip (lines strA) (lines strB)

iterateM_ :: Monad m => (a -> m a) -> a -> m b
iterateM_ f a = do res <- f a
                   iterateM_ f res

none :: [Bool] -> Bool
none = and . map not
