module PPTTestUtil where

import ProbProcessTree

putTree :: (Show a) => PPTree a -> IO()
putTree x = putStrLn (formatPPTree x)


