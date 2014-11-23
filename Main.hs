module Main where

import Util

import Model
import Direction
import Elements
import SparseRead
import ShapeDetection

import System.Environment
import System.FilePath (replaceExtension, dropExtension)
import Data.Function (on)
import Data.List (sortBy)

---------- File emission
ss :: Integer -> String
ss = show . show . (*10)

svgShow :: Element -> String
svgShow (Line (x, y) (x', y')) = concat ["<line x1=", ss x, " y1=", ss y, " x2=", ss x', " y2=", ss y', " stroke-width=\"3\" stroke=\"green\"/>"]
svgShow (Ellipse a@(x, y) b@(x', y')) = concat [ "<ellipse cx=", ss cx, " cy=", ss cy, " rx=", ss rx, " ry=", ss ry
                                               , " stroke-width=\"3\" stroke=\"blue\" fill=\"transparent\"/>"]
    where (cx, cy) = midpoint a b
          rx = (x' - x) `div` 2
          ry = (y' - y) `div` 2

svgWrite :: FilePath -> [Element] -> IO ()
svgWrite _ [] = do return ()
svgWrite fname elems = writeFile fname contents
    where contents = concat [ "<svg width=\"11in\" height=\"8.5in\" "
                            , "viewBox=\"", s $ x-10, " ", s $ y-10, " ", s $ x'+10, " ", s $ y'+10, "\" "
                            , "xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\">"
                            , concatMap svgShow elems
                            , "</svg>" 
                            ]
          ((x, y), (x', y')) = foldl minimize first elems
          minimize (a', b') elem = (minC b (minC a a'), maxC a (maxC b b'))
              where (a, b) = ptsOf elem
          ptsOf (Line a b)    = (a, b)
          ptsOf (Ellipse a b) = (a, b)
          first = ptsOf $ head elems
          s = show . (*10)

fbShow :: String -> Element -> String
fbShow factId (Line (x, y) (x', y')) = 
    concat [ "(", factId, " :LINE-SEGMENT NIL)\n"
           , "(", factId, " :START (", show x, " ", show y, "))\n"
           , "(", factId, " :END (", show x', " ", show y', "))\n"
           ]
fbShow factId (Ellipse (x, y) (x', y')) =
    concat [ "(", factId, " :ELLIPSE NIL)\n"
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
           islandGroups = filter ((>threshold) . sizeI) . map trim $ concatMap islands colors
           ellipses = filter isEllipse islandGroups
           directions = map getDecision $ filter (not . isEllipse) islandGroups
           regions = map (sortBy (flip compare `on` sizeI) . filter ((>threshold) . sizeI) . concatMap islands . splitByVal) directions
           elems = concat [map computeEllipse ellipses, concatMap (computeElements threshold) regions]
           aligned = align alignThreshold elems
       putStrLn "### COLORS #######"
       writeFile (withExt "colors") $ concatMap showCharGrid colors
       putStrLn "### ISLANDS ######"
       writeFile (withExt "islands") $ unlines $ map showCharGrid islandGroups
       putStrLn "### ELLIPSES #####"
       writeFile (withExt "ellipses") $ unlines $ map showCharGrid ellipses
       putStrLn "### DIRECTIONS ###"
       writeFile (withExt "directions") . unlines $ map showGrid directions
--       mapM_ (putBeside . map showGrid) $ directions
       putStrLn "### REGIONS ######"
       writeFile (withExt "regions") . unlines $ map (concatMap showGrid) regions
--       mapM_ (putBeside . map showGrid) $ regions
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
