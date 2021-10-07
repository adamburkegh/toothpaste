module TPConform where

import ProbProcessTree
import TraceUtil

-- probability [0,1]
prob :: (Eq a, Ord a) => [a] -> PPTree a -> Float
prob s (NodeN Choice ptl w) =  sum (map (\u -> weight u * prob s u) ptl) / wt
    where wt = sum (map weight ptl)
prob s (NodeN Conc ptl w) = probConc s ptl
prob s (NodeN Seq  ptl w) = probSeq s ptl
prob s (Node1 FLoop pt r w) 
    = prob s (NodeN Seq (duplicate [pt] (round r)) (weight pt) ) 
prob s (Node1 PLoop pt r w) = probPLoop s pt r 
prob s (Leaf x w) | s == [x]    = 1
                  | otherwise = 0
prob s (Silent w) | s == []   = 1
                  | otherwise = 0


probConc :: (Eq a, Ord a) => [a] -> [PPTree a] -> Float
-- probConc s [pt] = prob s pt
probConc s ptl
        = (sum 
            ( map (\(pt1,ptl1) -> probConcElem s pt1 ptl1 ) 
                  (elemCompl ptl) ) ) 
            / wt
        where wt = sum (map weight ptl)


-- each element in the list, paired with the remainder of the list
elemCompl :: [a] -> [(a,[a])]
elemCompl (x:xs) = elemCompl2 [] x xs
elemCompl [] = []

elemCompl2 :: [a] -> a -> [a] -> [(a,[a])]
elemCompl2 xs y (z:zs) = [(y,xs++(z:zs))] ++ elemCompl2 (xs ++ [y]) z zs
elemCompl2 xs y [] = [(y,xs)]

probConcElem :: (Eq a, Ord a) => [a] -> PPTree a -> [PPTree a] -> Float
probConcElem s pt ptl = 
        w * (prob s (NodeN Seq [pt, concP ptl w] w) ) 
    where w  = weight pt

probSeq :: (Eq a, Ord a) => [a] -> [PPTree a] -> Float
probSeq s [pt]     = prob s pt
probSeq s (pt:ptl) = prob [] pt * probSeq s ptl
                   + probSeqS s 1 (pt:ptl)
    where w = weight pt
probSeq [] [] = warn ("Empty in probSeq (empty trace) " ) 1
probSeq s  [] = warn ("Empty in probSeq " ) 0

probSeqS :: (Eq a, Ord a) => [a] -> Int -> [PPTree a] -> Float
probSeqS s n (pt:ptl) 
    | n < length s  = prob fs pt * prob sn (NodeN Seq ptl w)
                      + probSeqS s (n+1) (pt:ptl)
    | n == length s = prob s pt * prob [] (NodeN Seq ptl w)
        where (fs,sn) = splitAt n s
              w       = weight pt
probSeqS s n ptl      = 0

probPLoop :: (Eq a, Ord a) => [a] -> PPTree a -> Float -> Float
probPLoop [] pt r = 1/ (r + (prob [] pt)*(1 - r))
probPLoop s  pt r = probPLoopL s 1 pt r 

probPLoopL :: (Eq a, Ord a) => [a] -> Int -> PPTree a -> Float -> Float
probPLoopL s n pt r 
    | n <  length s = nterm + probPLoopL s (n+1) pt r
    | n == length s = nterm
        where nterm = prob s (NodeN Seq (duplicate [pt] n) (weight pt) ) 
                    * (r - 1) ** (fromIntegral n)
                    / r ** (fromIntegral n+1)


