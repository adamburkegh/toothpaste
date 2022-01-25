{-# LANGUAGE ImplicitParams #-}

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


loud :: PPTree a -> Bool
loud (NodeN op ptl w) = all (loud) ptl
loud (Node1 FLoop pt r w) = loud pt
loud (Node1 PLoop pt r w) = False
loud (Leaf x w) = True
loud (Silent w) = False

-- default epsilon for approximations
defaulteps = 0.001

-- probability [0,1]
prob :: (Eq a, Ord a) => [a] -> PPTree a -> Float
prob s pt = let ?eps = defaulteps in probEps s pt 

-- probability [0,1]
probEps :: (Eq a, Ord a, ?eps :: Float) => [a] -> PPTree a -> Float 
probEps s (NodeN Choice ptl w) =  
    sum (map (\u -> weight u * probEps s u) ptl) / wt
    where wt = sum (map weight ptl)
probEps s (Leaf x w) | s == [x]    = 1
                     | otherwise = 0
probEps s (Silent w) | null s    = 1
                     | otherwise = 0
probEps s (NodeN Seq  ptl w) = probSeq s ptl
probEps s (Node1 FLoop pt r w) 
    = probEps s (NodeN Seq (duplicate [pt] (round r)) (weight pt) ) 
probEps s (NodeN Conc ptl w) = probEps s (pathsetConc (NodeN Conc ptl w) ?eps) 
probEps s (Node1 PLoop pt r w) = probPLoop s pt r ?eps



-- TODO dead code
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

-- probPLoop is an approximation for silent subtrees
-- sigma subtree reps epsilon
probPLoop :: (Eq a, Ord a) => [a] -> PPTree a -> Float -> Float -> Float
probPLoop [] pt r eps = (1/r) + (prob [] pt) / r**2
probPLoop s pt r eps | length s == 1 = prob s pt *((r-1)/r^2) 
                                     + prob s pt * (prob [] pt) / r**2  
                     | loud (pt) = (probLoudLoop s pt r) /r 
                     | otherwise = probPLoopApprox s pt r eps


probLoudLoop :: (Eq a, Ord a) => [a] -> PPTree a -> Float -> Float 
probLoudLoop s pt r = probPLoopNth s pt r 1 (length s)
                        
probPLoopApprox :: (Eq a, Ord a) => [a] -> PPTree a -> Float -> Float -> Float
probPLoopApprox s pt r eps = (probPLoopNth s pt r 1 k)/r
            where k = findLoopApproxK r eps

probPLoopNth ::  (Eq a, Ord a) => [a] -> PPTree a -> Float -> Int -> Int 
                                      -> Float 
probPLoopNth s pt r i n | i < n  = pli + probPLoopNth s pt r (i+1) n
                        | i >= n = pli 
            where pli = ((r-1)/r)^i * (prob s (Node1 FLoop pt (fromIntegral i) 
                                                               (weight pt)) )
 
findLoopApproxK :: Float -> Float -> Int
findLoopApproxK r eps = findLoopApproxKAccum r eps (r-1) 1

findLoopApproxKAccum :: Float -> Float -> Float -> Int -> Int
findLoopApproxKAccum r eps cum i 
    | nc  <  eps = i
    | nc  >= eps = findLoopApproxKAccum r eps nc (i+1)
    where nc = cum*(r-1)/r 

-- pathsets are represented as PPTs
isPathset :: PPTree a -> Bool
isPathset (NodeN Conc ptl w) = False
isPathset (NodeN op ptl w) = all (isPathset) ptl
isPathset (Node1 FLoop pt r w) = False
isPathset (Node1 PLoop pt r w) = False
isPathset (Leaf x w) = True
isPathset (Silent w) = True

pathset :: (Ord a) => PPTree a -> PPTree a
pathset pt = pathsetEps pt defaulteps

pathsetEps :: (Ord a) =>  PPTree a -> Float -> PPTree a
pathsetEps (NodeN Seq ptl w) eps = NodeN Seq (map (`pathsetEps` eps) ptl) w
pathsetEps (NodeN Choice ptl w) eps = NodeN Choice
                                            (map (`pathsetEps` eps) ptl) w
pathsetEps (NodeN Conc ptl w) eps = pathsetConc (NodeN Conc ptl w) eps
pathsetEps (Node1 FLoop pt r w) eps = 
                    NodeN Seq 
                          (duplicate (map (`pathsetEps` eps) [pt]) (round r)) 
                          (weight pt) 
pathsetEps (Node1 PLoop pt r w) eps = pathsetPLoop pt r eps k
    where k = findLoopApproxK r eps
pathsetEps pt eps = pt

pathsetPLoop :: (Ord a) => PPTree a -> Float -> Float -> Int -> PPTree a
pathsetPLoop pt r eps k = NodeN Seq [Silent w,
                                     NodeN Choice 
                                           (pathsetPLoopList pt r eps k []) w] 
                                     w
             where w = weight pt

-- This would be better as a deterministic prefix tree
pathsetPLoopList :: (Ord a) => PPTree a -> Float -> Float -> Int -> [PPTree a] 
                             -> [PPTree a]
pathsetPLoopList pt r eps n ptl 
        | i < n     = pathsetPLoopList pt r eps n (ipt:ptl)
        | otherwise = (ipt:ptl)
            where i   = length ptl 
                  j   = fromIntegral i
                  sf  = (r-1)**j/(r**(j+1))
                  w   = weight pt
                  ipt = scale (seqP ((duplicate (map (`pathsetEps` eps) 
                                                    [pt]) 
                                                    i) 
                                     ++ [Silent w] ) 
                                    w)
                              sf


pathsetConc :: (Ord a) => PPTree a -> Float -> PPTree a
pathsetConc (NodeN Conc ptl w) eps = 
    NodeN Seq [Silent w,psConcChild (map (\pt -> pathsetEps pt eps) ptl)] w
pathsetConc s eps = warn "Non conc operator passed to pathsetConc" emptyTree

-- pre: all isPathset ptl
psConcChild :: (Ord a) => [PPTree a] -> PPTree a
psConcChild ptl = choiceP (map psConcChildElem (elemCompl ptl)) 
                              (sum (map weight ptl))

psConcChildElem :: (Ord a) => (PPTree a,[PPTree a]) -> PPTree a
psConcChildElem (pt,[]) = pt
psConcChildElem ((Leaf x w),ptl) = NodeN Seq [Leaf x w,psConcTail ptl w] w
psConcChildElem ((Silent w),ptl) = NodeN Seq [Silent w,psConcTail ptl w] w
-- psConcChildElem ((NodeN Seq (spt:sptl) w),ptl) = 
    -- NodeN Seq [spt, NodeN Choice  w] w
-- pathsetEps needed on these at some point, this covers seq of leaves only
-- choice and sequence
psConcChildElem (pt,ptl) = warn "Non conc operator passed to psConcChildElem" 
                                emptyTree

-- TODO partial
psConcTail :: (Ord a) => [PPTree a] -> Weight -> PPTree a
psConcTail [] w = warn "Empty seq passed to psConcTail" emptyTree
psConcTail [pt] w = replaceWeight pt w
psConcTail (pt:ptl) w =  psConcChild (map (`scale` (w/sw)) 
                                          (pt:ptl)  )
    where sw = sum (map weight (pt:ptl))

-- pre: sorted order
shuffle :: (Eq a, Ord a) => PPTree a -> PPTree a -> PPTree a
shuffle (Leaf x1 w1) (Leaf x2 w2) = shuffleSingles (Leaf x1 w1) (Leaf x2 w2) 
shuffle (Leaf x1 w1) (Silent w2)  = shuffleSingles (Leaf x1 w1) (Silent w2)
shuffle (Silent w1)  (Silent w2)  = shuffleSingles (Silent w1) (Silent w2)
shuffle (Leaf x1 w1) (NodeN Seq ptl w2) = 
    shuffleSingleSeq (Leaf x1 w1) (NodeN Seq ptl w2) 
shuffle (Silent w1)  (NodeN Seq ptl w2) = 
    shuffleSingleSeq (Silent w1) (NodeN Seq ptl w2) 
shuffle (NodeN Seq ptl1 w1) (NodeN Seq ptl2 w2) = 
    shuffleSeq ptl1 ptl2
shuffle x y = emptyTree



shuffleSingles :: (Eq a,Ord a) => PPTree a -> PPTree a -> PPTree a
shuffleSingles pt1 pt2 = 
               choiceP [NodeN Seq [pt1,scale pt2 (w1/w2) ] w1,
                        NodeN Seq [pt2,scale pt1 (w2/w1) ] w2] w
    where w1 = weight pt1
          w2 = weight pt2
          w  = w1+w2

shuffleSingleSeq :: (Eq a,Ord a) => PPTree a -> PPTree a -> PPTree a
shuffleSingleSeq pt (NodeN Seq [spt] w)
    = shuffleSingles pt spt
shuffleSingleSeq pt (NodeN Seq (spt:sptl) w2)
    = choiceP [NodeN Seq ([pt] ++ map (`scale` (w1/w2)) (spt:sptl)) w1,
               NodeN Seq [spt,
                          shuffleSingleSeq (scale pt (w2/w)) 
                                           (scale (NodeN Seq sptl w2) 
                                                  (w2/w)) ]
                     w2] w
        where w1 = weight pt
              w  = w1+w2
shuffleSingleSeq _ _ = warn "No match" emptyTree
                    
shuffleSeq :: (Eq a,Ord a) => [PPTree a] -> [PPTree a] -> PPTree a
shuffleSeq [pt1] y = shuffle pt1 (NodeN Seq y (weight (head y)))
shuffleSeq x [pt2] = shuffle pt2 (NodeN Seq x (weight (head x)))
shuffleSeq (pt1:ptl1) (pt2:ptl2) 
    = choiceP [NodeN Seq [pt1, scale (shuffleSeq ptl1 (pt2:ptl2)) (w1/w)] w1,
               NodeN Seq [pt2, scale (shuffleSeq (pt1:ptl1) ptl2) (w2/w)] w2] 
              w
         where w1 = weight pt1
               w2 = weight pt2
               w = w1+w2

-- Choice - pick both (with op) and scale? Or?


