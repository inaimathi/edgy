module Main where

import Util

import Model
import Direction
import Elements
import SparseRead

import System.Environment
import System.FilePath (replaceExtension, dropExtension)
import Data.Function (on)
import Data.List (sortBy)

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
test :: IO ()
test = do mapM_ (processFile 3 6) [ "test-data/single-color.txt"
                                  , "test-data/multi-color.txt"
                                  , "test-data/pentagon.txt"
                                  ]
          processFile 10 15 "test-data/circle-arrow-rect.ppm"
          processFile 0 10 "test-data/sanitized-input.ppm"

processFile :: Integer -> Integer -> FilePath -> IO ()
processFile alignThreshold threshold fname = 
    do f <- readSparse fname
       putStrLn $ "\n=>" ++ fname
       let colors = splitByVal f
           islandGroups = concatMap (islands threshold) colors
           directions = map getDirections islandGroups
           regions = map (sortBy (flip compare `on` sizeI) . concatMap (islands threshold) . concatMap splitByVal) directions
           elems = concatMap (computeElements threshold) regions
           aligned = align alignThreshold elems
       putStrLn "### COLORS #######"
       writeFile (withExt "colors") $ concatMap showCharGrid colors
       putStrLn "### DIRECTIONS ###"
       writeFile (withExt "directions") . unlines $ map (concatMap showGrid) directions
--       mapM_ (putBeside . map showGrid) $ directions
       putStrLn "### REGIONS ######"
       writeFile (withExt "regions") . unlines $ map (concatMap showGrid) regions
       mapM_ (putBeside . map showGrid) $ regions
       putStrLn "### ELEMS ########"
       writeFile (withExt "elems") . unlines $ map show elems
       putStrLn "### ALIGNED ######"
       writeFile (withExt "aligned") . unlines $ map show aligned
       fbWrite (withExt "base") aligned
       svgWrite (withExt "svg") aligned
           where withExt = replaceExtension fname

main :: IO ()
main = do args <- getArgs
          case args of
            (at:t:rest) -> mapM_ (processFile (read at) (read t)) rest
            _ -> putStrLn "Usage: <program> :: <align-threshold> <threshold> [<filename>]"
