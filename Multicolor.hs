module Multicolor where

import Util
import SparseRead
import Elements
import Direction

import qualified Data.Map as Map

main = do f <- readSparse "multi.txt"
          let separated = concatMap (islands 7) . splitByVal $ gridMap f
              direct fn = map (showMap . Map.map fn . scoreMap)
              directed = concat [direct cardinal separated, direct ordinal separated]
          mapM_ putBeside . splitEvery 3 $ directed
