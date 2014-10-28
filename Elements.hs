module Elements ( Element(..)
                , fbShow, fbWrite) where

import SparseRead

data Element = Line Coord Coord deriving (Eq, Ord, Show, Read)

fbShow :: String -> Element -> String
fbShow factId (Line (x, y) (x', y')) = 
    concat [ "(", factId, " :LINE NIL)\n"
           , "(", factId, " :START (", show x, " ", show y, "))\n"
           , "(", factId, " :END (", show x', " ", show y', "))\n"]
    
fbWrite :: FilePath -> [Element] -> IO ()
fbWrite fname elems = writeFile fname . concat $ map (uncurry fbShow) pairs
    where pairs = zip (map ((":FACT"++) . show) [1..]) elems
