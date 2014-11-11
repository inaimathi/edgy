module Main where

import Model
import Elements
import SparseRead

import System.FilePath (replaceExtension, dropExtension)

---------- File emission
svgShow :: Element -> String
svgShow (Line (x, y) (x', y')) = concat ["<line x1=", ss x, " y1=", ss y, " x2=", ss x', " y2=", ss y', " stroke-width=\"3\"/>"]
    where ss = show . show . (*10)

svgWrite :: FilePath -> [Element] -> IO ()
svgWrite _ [] = do return ()
svgWrite fname elems = writeFile fname contents
    where contents = concat [ "<svg width=\"11in\" height=\"8.5in\" "
                            , "viewBox=\"", s $ x-10, " ", s $ y-10, " ", s $ x'+10, " ", s $ y'+10, "\" "
                            , "xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\">"
                            , "<g stroke=\"green\">"
                            , concatMap svgShow elems
                            , "</g></svg>" 
                            ]
          ((x, y), (x', y')) = foldl (\(a', b') (Line a b) -> (minC b (minC a a'), maxC a (maxC b b'))) first elems
          first = let (Line a b) = head elems
                  in (a, b)
          s = show . (*10)

fbShow :: String -> Element -> String
fbShow factId (Line (x, y) (x', y')) = 
    concat [ "(", factId, " :LINE-SEGMENT NIL)\n"
           , "(", factId, " :START (", show x, " ", show y, "))\n"
           , "(", factId, " :END (", show x', " ", show y', "))\n"
           ]

fbWrite :: FilePath -> [Element] -> IO ()
fbWrite fname elems = writeFile fname contents
    where pairs = zip (map ((":FACT"++) . show) [2..]) elems
          contents = concat $ meta : (map (uncurry fbShow) pairs)
          meta = concat [ "(:FACT0 :NEXT-ID NIL)\n"
                        , "(:FACT0 :VALUE ", show $ 2 + length elems, ")\n"
                        , "(:FACT1 :DIAGRAM-NAME NIL)\n"
                        , "(:FACT1 :VALUE ", show $ dropExtension fname, ")\n"
                        ]

---------- Main and related utility
main :: IO ()
main = do mapM_ (processFile 3 6) [ "test-data/single-color.txt"
                                  , "test-data/multi-color.txt"
                                  , "test-data/pentagon.txt"
                                  , "test-data/pentagon-multiline-to-square.txt"
                                  , "test-data/circle-arrow-rect.txt" ]
          processFile 10 15 "test-data/circle-arrow-rect.ppm"
          processFile 3 20 "test-data/sanitized-input.ppm"

processFile :: Integer -> Integer -> FilePath -> IO ()
processFile alignThreshold threshold fname = do f <- readSparse fname
                                                putStrLn $ "\n=>" ++ fname
                                                let elems = align alignThreshold $ getElements threshold f
                                                fbWrite (replaceExtension fname "base") elems
                                                svgWrite (replaceExtension fname "svg") elems
                                                mapM_ (putStrLn . show) elems
