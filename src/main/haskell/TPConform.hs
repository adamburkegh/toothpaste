{-# LANGUAGE ImplicitParams #-}

module TPConform where

import ProbProcessTree
import TraceUtil

import Data.List (sort)


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

-- Prefix Tree
data PFTree a = PFNode (PFToken a) [PFTree a] Weight deriving (Show,Ord)

instance (Eq a) => Eq (PFTree a) where
    PFNode t1 pcl1 w1 == PFNode t2 pcl2 w2 = 
        t1 == t2 && pcl1 == pcl2 && w1 == w2

data PFToken a = PFSymbol a | PFSilent | PFNull
    deriving (Show,Eq,Ord)

pfleaf :: a -> Weight -> PFTree a
pfleaf x w = PFNode (PFSymbol x) [] w

pfsilent :: Weight -> PFTree a
pfsilent w = PFNode PFSilent [] w

pfweight :: PFTree a -> Weight
pfweight (PFNode t ctl w) = w

pfnorm :: (Ord a) => PFTree a -> PFTree a
pfnorm (PFNode t ctl w) = PFNode t 
                                 (map pfnorm 
                                      (sort ctl) ) w


formatPFTree :: (Show a) => PFTree a -> String 
formatPFTree pf = formatPFTreeIndent pf 0

formatPFTreeIndent :: (Show a) => PFTree a -> Int -> String 
formatPFTreeIndent (PFNode t ctl w) indent = 
    duplicate indentStr indent ++ formatToken t 
    ++ formatWeight w
    ++ concatMap (\pf -> formatPFTreeIndent pf (indent+1) ) ctl

formatToken :: (Show a) => PFToken a -> String
formatToken (PFSymbol t) = show t
formatToken PFSilent = "tau"
formatToken PFNull = "=X="


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
probEps s (NodeN Conc ptl w) = 
    pfprob s (pathset (NodeN Conc ptl w) ?eps)
probEps s (Node1 PLoop pt r w) = probPLoop s pt r ?eps



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

--
-- pathset impl using PFTrees
pathset :: (Eq a, Ord a) => PPTree a -> Float -> PFTree a
pathset (Leaf x w) eps        = PFNode (PFSymbol x) [] w
pathset (Silent w) eps        = PFNode PFSilent [] w
pathset (NodeN Seq (pt:ptl) w) eps =
    pfappend (pathset pt eps)
             (map (`pathset` eps) ptl) 
pathset (NodeN Choice ptl w) eps = PFNode PFNull (map (`pathset` eps) ptl) w
pathset (NodeN Conc ptl w) eps   = ps2Conc (NodeN Conc ptl w) eps 
pathset (NodeN op [] w) eps   = 
    warn "empty NodeN children in pathset" PFNode PFNull [] w
pathset (Node1 FLoop pt r w) eps = 
    pfappend pf (duplicate [pf] ((round r)-1)) 
    where pf = pathset pt eps
pathset (Node1 PLoop pt r w) eps = 
    PFNode PFSilent ((pfsilent (w/r)):npf:(ps2PLoop pf pf (k-1) nw r)) w
    where k  = findLoopApproxK r eps
          (PFNode t ctl w1) = pathset pt eps
          pf  = PFNode t ctl w1
          nw  = ((w1*(r-1))/(r*r))
          npf = PFNode t ctl nw


deadPFTree = PFNode PFNull [] 1

pfappend :: PFTree a -> [PFTree a] -> PFTree a
pfappend (PFNode x pcl w) []        = PFNode x pcl w
pfappend (PFNode x [] w) (pf:pfl)   = PFNode x [pfappend pf pfl] w
pfappend (PFNode x pcl w) (pf:pfl)  = PFNode x (map (\p -> pfappend p (pf:pfl))
                                                    pcl) w

ps2PLoop :: PFTree a -> PFTree a -> Int -> Weight -> Float -> [PFTree a]
ps2PLoop pf cumpf 0 w r = []
ps2PLoop (PFNode x pcl w) cumpf k cw r = 
    npf:(ps2PLoop pf ncumpf (k-1) nw r) 
    where pf      = PFNode x pcl w
          npf     = pfappend (PFNode x pcl nw) [cumpf]
          nw      = (cw*(r-1)/r)
          ncumpf  = pfappend pf [cumpf]

ps3 :: (Eq a, Ord a) => PPTree a -> PFTree a
ps3 pt = pathset pt defaulteps

ps2Conc :: (Eq a, Ord a) => PPTree a -> Float -> PFTree a
ps2Conc (NodeN Conc ptl w) eps = pfappend (PFNode PFSilent ctl w) [pfsilent w]
    where (PFNode t ctl sw) = pfshuffleList (map (\p -> pathset p eps) ptl)

-- collapse nulls not in parent
pfcollapse :: PFTree a -> PFTree a
pfcollapse (PFNode PFNull [c] w) = pfcollapse c
pfcollapse (PFNode t ctl w) = PFNode t (concat (map pfChildCollapse ctl)) w 

pfChildCollapse :: PFTree a -> [PFTree a]
pfChildCollapse (PFNode PFNull ctl w) = concat (map pfChildCollapse ctl)
pfChildCollapse (PFNode t ctl w)      = 
    [PFNode t (concat (map pfChildCollapse ctl)) w]


pfshuffle :: (Eq a) => PFTree a -> PFTree a -> PFTree a
pfshuffle pf1 pf2 = pfcollapse (PFNode PFNull 
                                       (pflshuffle pf1 pf2) (w1+w2) )
    where w1 = pfweight pf1
          w2 = pfweight pf2

pfshuffleList :: (Eq a) => [PFTree a] -> PFTree a
-- pfshuffleList (pf1:pf2:pfl) = pfshuffle pf1 (pfshuffleList (pf2:pfl))
pfshuffleList (pf1:pf2:pfl) = foldr pfshuffle pf1 (pf2:pfl)
pfshuffleList [pf] = pf
pfshuffleList []   = warn "empty list passed to shuffle" deadPFTree


pflshuffle :: (Eq a) => PFTree a -> PFTree a -> [PFTree a]
pflshuffle pf1 pf2 = [pfs pf1 pf2,pfs pf2 pf1] 

pfs :: (Eq a) => PFTree a -> PFTree a -> PFTree a
pfs (PFNode PFNull [] w1) pf2 = pf2
pfs (PFNode t1 [] w1) (PFNode PFNull ctl2 w12) = 
    PFNode t1 ctl2 w1
pfs (PFNode t1 [] w1) (PFNode t2 ctl2 w12) = 
    PFNode t1 [(PFNode t2 ctl2 w12)] w1
pfs (PFNode PFNull (c1:ctl1) w1) (PFNode t2 ctl2 wl2) =
    PFNode PFNull
           (map (\p -> pfs p pf2)
                   (c1:ctl1))
           w1
    where pf2 = (PFNode t2 ctl2 wl2)
pfs (PFNode t1 (c1:ctl1) w1) (PFNode t2 ctl2 w12) = 
    PFNode t1 
           ((pfs pf2 (PFNode PFNull (c1:ctl1) w1))
           :(map (\p -> pfs p pf2) (c1:ctl1) ) )
           w1
    where pf2 = (PFNode t2 ctl2 w12)


-- prefix tree prob
pfprob :: (Eq a) => [a] -> PFTree a -> Float
pfprob (sh:st) (PFNode (PFSymbol x) (n:ns) w) 
    | ph == 0   = 0
    | otherwise = ph * (sum (map (\c -> (pfprob st c) * (pfweight c) / ct)  
                                 (n:ns) ) )    
    where ph = pftokenprob [sh] (PFSymbol x)
          ct = sum (map pfweight (n:ns))
pfprob (sh:st) (PFNode (et) (n:ns) w) 
    | et == PFSilent || et == PFNull
    = (sum (map (\c -> (pfprob (sh:st) c) * (pfweight c) / ct)  
                            (n:ns) ) )    
    where ct = sum (map pfweight (n:ns))
pfprob s  (PFNode x [] w) = pftokenprob s x
pfprob [] (PFNode x cfl w) 
    | ph == 0   = 0
    | otherwise = ph * (sum (map (\c -> (pfprob [] c) * (pfweight c) / ct)
                                 cfl ) )
    where ph = pftokenprob [] x 
          ct = sum (map pfweight cfl)

pftokenprob :: (Eq a) => [a] -> PFToken a -> Float
pftokenprob s (PFSymbol x) | s == [x]  = 1
                           | otherwise = 0
pftokenprob s (PFSilent)   | s == []   = 1
                           | otherwise = 0
pftokenprob s (PFNull)     | s == []   = 1
                           | otherwise = 0


