module TPConform where

import ProbProcessTree
import TraceUtil


-- convenience
log2 :: Floating a => a -> a
log2 x = logBase 2 x

-- permutation utilities

-- each element in the list, paired with the remainder of the list
elemCompl :: [a] -> [(a,[a])]
elemCompl (x:xs) = elemCompl2 [] x xs
elemCompl [] = []

elemCompl2 :: [a] -> a -> [a] -> [(a,[a])]
elemCompl2 xs y (z:zs) = [(y,xs++(z:zs))] ++ elemCompl2 (xs ++ [y]) z zs
elemCompl2 xs y [] = [(y,xs)]


-- probability [0,1]
prob :: (Eq a, Ord a) => [a] -> PPTree a -> Float
prob s (NodeN Choice ptl w) =  sum (map (\u -> weight u * prob s u) ptl) / wt
    where wt = sum (map weight ptl)
prob s (NodeN Conc ptl w) = probConc s ptl
prob s (NodeN Seq  ptl w) = probSeq s ptl
prob s (Node1 FLoop pt r w) 
    = prob s (NodeN Seq (duplicate [pt] (round r)) (weight pt) ) 
prob s (Node1 PLoop pt r w) = 0 -- probPLoop s pt r TODO BROKEN
prob s (Leaf x w) | s == [x]    = 1
                  | otherwise = 0
prob s (Silent w) | s == []   = 1
                  | otherwise = 0


probConc :: (Eq a, Ord a) => [a] -> [PPTree a] -> Float
probConc s ptl
        = (sum 
            ( map (\(pt1,ptl1) -> probConcElem s pt1 ptl1 ) 
                  (elemCompl ptl) ) ) 
            / wt
        where wt = sum (map weight ptl)

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
probPLoop [] pt r = 1/ (r - ((prob [] pt)*(r-1)))
probPLoop s pt r = 1/ (r - ((prob s pt)*(r-1))) - (1/r)


-- entropy
-- broken, not in use
entropy :: (Ord a) => PPTree a -> Float
entropy (Leaf _ _) = 0
entropy (Silent _) = 0
{-
entropy (NodeN Seq ptl w) = sum $ map entropy ptl
entropy (NodeN Choice ptl w) = 
        -1 * (sum (map (\pt -> (weight pt) * log2 ((weight pt)/wt )) ptl)) / wt 
        + (sum $ map (\pt -> (weight pt) * (entropy pt) ) ptl ) / wt
    where wt = sum $ map weight ptl
entropy (NodeN Conc ptl w) =
    ( sum ( map (\(pti,ptli) -> entropyConcElem pti ptli wt) 
                (elemCompl ptl)  ) ) 
        / wt
    where  wt = sum $ map weight ptl
entropy (Node1 FLoop pt r w) = r * entropy pt
entropy (Node1 PLoop pt r w) = (entropy pt - log2((r-1)/r)) 
                   * (r / ((r-1) ** 2) )
                   - ( log2(1/r) /(r-1)  )

entropyConcElem :: (Ord a) => PPTree a -> [PPTree a] -> Float -> Float
entropyConcElem pt ptl wt =  
      w * ( (entropy pt) 
          + (entropy 
                (concP ptl (sum $ map weight ptl) ) ) 
          - (log2 ( w/ wt) )  ) 
    where w = weight pt
-}

