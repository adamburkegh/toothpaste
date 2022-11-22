module PPTTestUtil where

import ProbProcessTree

putTree :: (Show a) => PPTree a -> IO()
putTree x = putStrLn (formatPPTree x)

eps =  0.0001

(~~~) :: Float -> Float -> Bool
(~~~) x y = (x-y) < eps

acmp :: (Eq a) => PPTree a -> PPTree a -> Bool
acmp (Leaf a n) (Leaf b m) = a == b && n ~~~ m
acmp (Silent n) (Silent m) = n ~~~ m
acmp (Node1 p1 a r1 n) (Node1 p2 b r2 m)
        = (p1 == p2) && acmp a b && (r1 ~~~ r2) && (n ~~~ m)
acmp (NodeN po1 ptl1 n) (NodeN po2 ptl2 m)
        = (po1 == po2) && acmpl ptl1 ptl2 && (n ~~~ m)
acmp x y = False

acmpl :: (Eq a) => [PPTree a] -> [PPTree a] -> Bool
acmpl ptl1 ptl2 =  length ptl1  == length ptl2
        && foldl (\c (pt1,pt2) -> acmp pt1 pt2 && c) True z
    where z = zip ptl1 ptl2


