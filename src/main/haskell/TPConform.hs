module TPConform where

import ProbProcessTree
import TraceUtil


-- convenience
log2 :: Floating a => a -> a
log2 = logBase 2 

-- permutation utilities

-- each element in the list, paired with the remainder of the list
elemCompl :: [a] -> [(a,[a])]
elemCompl (x:xs) = elemCompl2 [] x xs
elemCompl [] = []

elemCompl2 :: [a] -> a -> [a] -> [(a,[a])]
elemCompl2 xs y (z:zs) = (y,xs++(z:zs)) : elemCompl2 (xs ++ [y]) z zs
elemCompl2 xs y [] = [(y,xs)]

headify :: a -> [[a]] -> [[a]]
headify h = map (h:) 

permute :: [a] -> [[a]]
permute (x:xs) = concatMap (\(y,yl) -> headify y (permute yl)  ) 
                              (elemCompl (x:xs))
permute []     = [[]]

-- ordered sub projections
ordSubProj :: [a] -> [[a]]
ordSubProj (x:xs) = ordSubProj2 [x] xs
ordSubProj []     = []

ordSubProj2 :: [a] -> [a] -> [[a]]
ordSubProj2 pref (x:xs) = ordSubProj2 (pref++[x]) xs ++ ordSubProj2 pref xs
ordSubProj2 pref []     = [pref]

-- ordered sub projections, paired with complement
ordSubProjPairs :: [a] -> [([a],[a])]
ordSubProjPairs (x:xs) = ordSubProjPairs2 [x] [] xs
ordSubProjPairs []     = []

ordSubProjPairs2 :: [a] -> [a] -> [a] -> [([a],[a])]
ordSubProjPairs2 pref pref2 (x:xs) = b1 ++ b2
    where b1 = ordSubProjPairs2 (pref++[x]) pref2 xs
          b2 = ordSubProjPairs2 pref (pref2++[x]) xs
ordSubProjPairs2 pref pref2 []     = [(pref,pref2)]


-- probability [0,1]
prob :: (Eq a, Ord a) => [a] -> PPTree a -> Float
prob s (NodeN Choice ptl w) =  sum (map (\u -> weight u * prob s u) ptl) / wt
    where wt = sum (map weight ptl)
prob s (NodeN Conc ptl w) = 0 -- probConc s ptl TODO BROKEN
prob s (NodeN Seq  ptl w) = probSeq s ptl
prob s (Node1 FLoop pt r w) 
    = prob s (NodeN Seq (duplicate [pt] (round r)) (weight pt) ) 
prob s (Node1 PLoop pt r w) = 0 -- probPLoop s pt r TODO BROKEN
prob s (Leaf x w) | s == [x]    = 1
                  | otherwise = 0
prob s (Silent w) | null s    = 1
                  | otherwise = 0


probConc :: (Eq a, Ord a) => [a] -> [PPTree a] -> Float
probConc s [pt] = prob s pt
-- probConc s (pt:ptl) =   probConcC s 1 (pt:ptl)
--                       + probConcSplits [] s (pt:ptl) 
probConc s (pt:ptl)
        = sum( map (\ss -> probConcC ss 1 (pt:ptl)
                         + probConcSplits [] ss (pt:ptl) )
                   pms ) / (fromIntegral $ length pms)
          where pms = permute s

probConcSplits :: (Eq a, Ord a) => [a] -> [a] -> [PPTree a] -> Float
probConcSplits s1 s2 ptl = sum( map (\(u,uptl) -> (weight u)
                                       * prob s1 u
                                       * probConc s2 uptl )
                                (elemCompl ptl) )
                           / wt
               where wt = sum (map weight ptl) 

probConcC :: (Eq a, Ord a) => [a] -> Int -> [PPTree a] -> Float
probConcC s n (pt:ptl) 
    | n <  length s =   probConcSplits fs sn (pt:ptl)  
                      + probConcC s (n+1) (pt:ptl)
    | n == length s = probConcSplits s  [] (pt:ptl) 
     where  (fs,sn) = splitAt n s
probConcC s n ptl = 0 

probSeq :: (Eq a, Ord a) => [a] -> [PPTree a] -> Float
probSeq s [pt]     = prob s pt
probSeq s (pt:ptl) = prob [] pt * probSeq s ptl
                   + probSeqS s 1 (pt:ptl)
    where w = weight pt
probSeq [] [] = warn "Empty in probSeq (empty trace) "  1
probSeq s  [] = warn "Empty in probSeq "  0

probSeqS :: (Eq a, Ord a) => [a] -> Int -> [PPTree a] -> Float
probSeqS s n (pt:ptl) 
    | n < length s  = prob fs pt * prob sn (NodeN Seq ptl w)
                      + probSeqS s (n+1) (pt:ptl)
    | n == length s = prob s pt * prob [] (NodeN Seq ptl w)
        where (fs,sn) = splitAt n s
              w       = weight pt
probSeqS s n ptl      = 0

probPLoop :: (Eq a, Ord a) => [a] -> PPTree a -> Float -> Float
probPLoop [] pt r = 1/ (r - (prob [] pt*(r-1)))
probPLoop s pt r = 1/ (r - (prob s pt*(r-1))) - (1/r)


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

