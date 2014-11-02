module Main where

import Model
import Elements
import SparseRead

import System.FilePath (replaceExtension)

---------- File emission
svgShow :: Element -> String
svgShow (Line (x, y) (x', y')) = concat ["<line x1=", ss x, " y1=", ss y, " x2=", ss x', " y2=", ss y', " stroke-width=\"2\"/>"]
    where ss = show . show . (*10)

svgWrite :: FilePath -> [Element] -> IO ()
svgWrite _ [] = do return ()
svgWrite fname elems = writeFile fname contents
    where contents = concat [ "<svg width=\"11in\" height=\"8.5in\" "
                            , "viewBox=\"", s $ x-10, " ", s $ y-10, " ", s $ x'+10, " ", s $ y'+10, "\" "
                            , "xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\">"
                            , "<g stroke=\"green\">"
                            , concatMap svgShow elems
                            , "</g></svg>" ]
          ((x, y), (x', y')) = foldl (\(a', b') (Line a b) -> (minC b (minC a a'), maxC a (maxC b b'))) first elems
          first = let (Line a b) = head elems
                  in (a, b)
          s = show . (*10)

fbShow :: String -> Element -> String
fbShow factId (Line (x, y) (x', y')) = 
    concat [ "(", factId, " :LINE-SEGMENT NIL)\n"
           , "(", factId, " :START (", show x, " ", show y, "))\n"
           , "(", factId, " :END (", show x', " ", show y', "))\n"]

fbWrite :: FilePath -> [Element] -> IO ()
fbWrite fname elems = writeFile fname . concat $ map (uncurry fbShow) pairs
    where pairs = zip (map ((":FACT"++) . show) [1..]) elems

---------- Main and related utility
processFile :: Integer -> FilePath -> IO ()
processFile threshold fname = do f <- readSparse fname
                                 putStrLn $ "\n=>" ++ fname
                                 let alignThresh = threshold `div` 2
                                     elems = align alignThresh $ getElements threshold f
                                 fbWrite (replaceExtension fname "base") elems
                                 svgWrite (replaceExtension fname "svg") elems
                                 mapM_ (putStrLn . show) elems

main :: IO ()
main = mapM_ (uncurry processFile) [ (6, "test-data/single-color.txt")
                                   , (6, "test-data/multi-color.txt")
                                   , (6, "test-data/pentagon.txt")
                                   , (6, "test-data/pentagon-multiline-to-square.txt")
                                   , (6, "test-data/circle-arrow-rect.txt")]