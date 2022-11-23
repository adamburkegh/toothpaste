module Toothpaste where

import ProbProcessTree
import Debug.Trace
import Data.List(partition,sort)

-- debug and trace
debugOn :: String -> a -> a
debugOn = trace

debugOff :: String -> a -> a
debugOff s x = x

debug :: String -> a -> a
-- Debug ON
debug = debugOn

-- Debug OFF
-- debug = debugOff

warn :: String -> a -> a
warn = trace 


-- Rule types

type PRule a = PPTree a -> PPTree a
type LRule a = [PPTree a] -> [PPTree a]
data TRule a = TRule{ rulename :: String, trule :: PRule a }
type PPTRuleTransform a = PPTree a -> [TRule a] -> PPTree a

type PSim a   = PPTree a -> PPTree a -> Bool
type PMerge a = PPTree a -> PPTree a -> PPTree a


-- Rules

-- Meta rule functions

-- consMerge simFunction mergeFunction childList
-- if simFunction on adjacent children, merge them
adjMerge :: (a->a->Bool) -> (a->a->a) -> [a] -> [a]
adjMerge simF mergeF (x:y:xs)
    | x `simF` y = adjMerge simF mergeF (x `mergeF` y:xs)
    | otherwise         = x: adjMerge simF mergeF (y:xs)
adjMerge sf mf [x] = [x]
adjMerge sf mf []  = []

-- if simFunction among any children, merge the first one
-- there is no guarantee that list members are still similar after merging
-- so we don't assume that (loopSim is an example where they may not be)
-- we merge only the first for conceptual simplicity
anyMerge :: (a->a->Bool) -> (a->a->a) -> [a] -> [a]
anyMerge simF mergeF (x:xs) 
    | null ml   = x:anyMerge simF mergeF xs
    | otherwise = nml ++ [mergeF x (head ml) ]
                      ++ tail ml
                      -- ++ anyMerge simF mergeF (tail ml)
    where (nml,ml) = break (\y -> x `simF` y) xs
anyMerge sf mf []  = []


-- Rules proper

silentSeq :: PRule a
silentSeq (NodeN Seq ((Silent _):pts) w) = silentSeq (NodeN Seq pts w)
silentSeq (NodeN Seq (pt:pts) w)         = NodeN Seq (pt:ptsr) w
            where NodeN Seq ptsr w2 = silentSeq (NodeN Seq pts w)
silentSeq x = x

silentConc :: PRule a
silentConc (NodeN Conc ((Silent _):pts) w) = silentConc (NodeN Conc pts w)
silentConc (NodeN Conc (pt:pts) w)         = NodeN Conc (pt:ptsr) w
            where NodeN Conc ptsr w2 = silentConc (NodeN Conc pts w)
silentConc x = x

-- Includes single node collapse and conc single node collapse in single rule 
-- in this impl
singleNodeOp :: PRule a
singleNodeOp (NodeN op [u] w)  = u
singleNodeOp x = x

choiceChildMR :: (Eq a, Ord a) => 
    LRule a -> PPTree a -> PPTree a
choiceChildMR crule (NodeN Choice ptl w) 
    | ptl /= cr  = choiceP cr w
    where cr = crule ptl
choiceChildMR crule x = x

choiceSim :: (Eq a, Ord a) => PRule a
choiceSim = choiceChildMR (adjMerge (=~=) merge)

loopSim :: (Eq a, Ord a) => PRule a
loopSim = choiceChildMR  (anyMerge (=&=) lmerge)

concSim :: Eq a => PRule a
concSim (NodeN Conc ptl w)
    | ptl /= cr = NodeN Conc cr w
    where cr = adjMerge (=~=) concMerge ptl
concSim x = x

concMerge :: PPTree a -> PPTree a -> PPTree a
concMerge u1 u2 = Node1 FLoop (merge u1 u2) 2 (w1+w2)
    where w1 = weight u1 
          w2 = weight u2

lconcMerge :: PPTree a -> PPTree a -> PPTree a
lconcMerge u1 u2 = Node1 FLoop (lmerge u1 u2) 2 (w1+w2)
    where w1 = weight u1 
          w2 = weight u2

loopConcSim :: Eq a => PRule a
loopConcSim (NodeN Conc ptl w)
    | ptl /= cr = NodeN Conc cr w
    where cr = anyMerge (=&=) lconcMerge ptl
loopConcSim x = x

fixedLoopRollSingle :: Eq a => PRule a
fixedLoopRollSingle (NodeN Seq ptl w)
    | ptl /= cr = seqP cr w
    where cr = adjMerge (==) (\u1 u2 -> Node1 FLoop u1 2 (weight u1) ) 
               ptl
fixedLoopRollSingle x = x

floopContEq :: Eq a => PPTree a -> PPTree a -> Bool
floopContEq (Node1 FLoop u1 r1 w1) (Node1 FLoop u2 r2 w2) = u1 == u2
floopContEq (Node1 FLoop u1 r w) u2 = u1 == u2
floopContEq u1 (Node1 FLoop u2 r w) = u1 == u2
floopContEq u1 u2                   = False

floopContMerge :: PPTree a -> PPTree a -> PPTree a
floopContMerge (Node1 FLoop u1 r1 w1) (Node1 FLoop u2 r2 w2) 
    = Node1 FLoop u1 (r1+r2) w1
floopContMerge (Node1 FLoop u1 r w) u2 = Node1 FLoop u1 (r+1) w
floopContMerge u1 (Node1 FLoop u2 r w) = Node1 FLoop u2 (r+1) w

fixedLoopRollExisting :: Eq a => PRule a
fixedLoopRollExisting (NodeN Seq ptl w)
    | ptl /= cr = seqP cr w
    where cr = adjMerge floopContEq floopContMerge ptl
fixedLoopRollExisting x = x

fixedLoopRoll :: (Eq a, Ord a) => PRule a
fixedLoopRoll pt = fixedLoopRollExisting $ fixedLoopRollSingle pt

-- remove when probLoopRoll converted to sim-merge style
loopRollEndPattern :: (Eq a) => PPTree a -> Float -> POper1 -> PPTree a
loopRollEndPattern prev ct poper
    | ct > 1  = Node1 poper prev ct (weight prev)
    | ct <= 1 = prev


-- Not in the paper and not currently used
-- no loops of subseq >= 2
probLoopRoll :: Eq a => PRule a
probLoopRoll (NodeN Seq (u1:ptl) w) 
    | nptl /= ptl = NodeN Seq nptl w
    where nptl = probLoopRollList ptl u1 1
probLoopRoll x = x    

probLoopRollList :: (Eq a) => [PPTree a] -> PPTree a -> Float -> [PPTree a]
probLoopRollList ((Node1 PLoop u1 r1 w1):ptl) prev ct
    | u1 =~= prev = probLoopRollList ptl (seqMerge u1 prev) (ct+r1-1)
    | not(u1 =~= prev) = probLoopRollEndPattern prev ct:
                           probLoopRollList ptl u1 r1
probLoopRollList (u1:ptl) prev ct 
    | u1 =~= prev            = probLoopRollList ptl (seqMerge u1 prev) (ct+1)
    | not (u1 =~= prev) = probLoopRollEndPattern prev ct:
                            probLoopRollList ptl u1 1
probLoopRollList [] prev ct = [probLoopRollEndPattern prev ct]
     

probLoopRollEndPattern :: (Eq a) => PPTree a -> Float -> PPTree a
probLoopRollEndPattern prev ct = loopRollEndPattern prev ct PLoop


loopFixToProb :: PRule a
loopFixToProb (Node1 FLoop x m w) = Node1 PLoop x m w
loopFixToProb x = x

loopNest :: PRule a
loopNest (Node1 FLoop (Node1 FLoop x r1 w1) r2 w2) = Node1 FLoop x (r1*r2) w2
loopNest (Node1 FLoop (Node1 PLoop x rp wp) rf wf)
    | rf > 1 = Node1 PLoop x (rp*(rf-1)) wf
loopNest (Node1 PLoop (Node1 op x rf wf) rp wp)
    | op == FLoop || op == PLoop = Node1 PLoop x (rf*rp) wp
loopNest x = x

loopGeo :: (Eq a, Ord a) => PRule a
loopGeo = choiceChildMR loopGeoList

loopGeoList :: (Eq a) => LRule a
loopGeoList ((Node1 FLoop u1 r1 w1):(Node1 FLoop u2 r2 w2):ptl) 
    | u1 =~= u2 = loopGeoList  (
                    Node1 PLoop (merge u1 u2) 
                                 (((r1*w1)+(r2*w2))/(w1+w2)) 
                                 (w1+w2) 
                    :ptl)
    | otherwise = (Node1 FLoop u1 r1 w1) : 
                    loopGeoList ( (Node1 FLoop u2 r2 w2) :ptl)
loopGeoList (pt1:ptl) = pt1:loopGeoList ptl
loopGeoList pt = pt


flattenRule :: (Eq a) => PRule a
flattenRule = flatten 

-- choice folds

headSimMS :: (Eq a, Ord a) => PSim a -> PSim a
headSimMS simf (NodeN Seq (pt1:ptl1) w1) (NodeN Seq (pt2:ptl2) w2) 
    = pt1 `simf` pt2
headSimMS sf pt1 pt2 = False

headMergeMM :: (Ord a) => PMerge a -> PMerge a
headMergeMM mergef (NodeN Seq (pt1:ptl1) w1) (NodeN Seq (pt2:ptl2) w2) 
    | null ptl1 && null ptl2 = mergef pt1 pt2 -- when though?
    -- | null ptl1 = Silent case
    | otherwise = seqP [mergef pt1 pt2,
            choiceP [ seqP ptl1 w1, seqP ptl2 w2 ] (w1+w2) ] (w1+w2)

choiceFoldMR :: (Eq a, Ord a) => LRule a -> PRule a
choiceFoldMR lrule (NodeN Choice ptl w)
    | ptl /= cr  = choiceP cr w
    where cr           = ptlg ++ ptlnf
          (ptlf,ptlnf) = partition isSeq ptl
          ptlg         = lrule ptlf
choiceFoldMR lrule x = x

choiceFoldPrefix  :: (Eq a, Ord a) => PRule a
choiceFoldPrefix = choiceFoldMR 
                    (adjMerge (headSimMS (=~=)) (headMergeMM merge))

-- O(n)
tailSimMS :: (Eq a, Ord a) => PSim a -> PSim a
tailSimMS simF (NodeN Seq (pt1:ptl1) w1) (NodeN Seq (pt2:ptl2) w2)
    = last (pt1:ptl1) `simF` last (pt2:ptl2)
tailSimMS simF pt1 pt2 = False

-- pre: non-empty
lastSplit :: [a] -> (a,[a])
lastSplit (x:y:xs)  = (lt,x:hs)
    where (lt,hs)   = lastSplit(y:xs)
lastSplit [x]       = (x,[])

-- pre: non-empty sequences
tailMergeMM :: (Ord a) => PMerge a -> PMerge a
tailMergeMM mergeF (NodeN Seq ptl1 w1) (NodeN Seq ptl2 w2) 
    = seqP [ choiceP [ seqP pttl1 w1, seqP pttl2 w2 ] (w1+w2), 
                       mergeF pt1 pt2 ] (w1+w2)
    where (pt1, pttl1) = lastSplit ptl1
          (pt2, pttl2) = lastSplit ptl2


choiceFoldSuffix :: (Eq a, Ord a) => PRule a
choiceFoldSuffix =  choiceFoldMR 
                        (anyMerge (tailSimMS (=~=)) (tailMergeMM merge))

loopChoiceFoldPrefix :: (Eq a, Ord a) => PRule a
loopChoiceFoldPrefix = choiceFoldMR
                        (anyMerge (headSimMS (=&=)) (headMergeMM lmerge))


-- O(n)
loopChoiceFoldSuffix :: (Eq a, Ord a) => PRule a
loopChoiceFoldSuffix = choiceFoldMR 
                        (anyMerge (tailSimMS (=&=)) (tailMergeMM lmerge))


-- choice skips


headLeafSimMS :: (Eq a, Ord a) => PSim a -> PSim a
headLeafSimMS simf pt1 (NodeN Seq (pt2:ptl2) w2) 
    = pt1 `simf` pt2
headLeafSimMS simf (NodeN Seq (pt2:ptl2) w2) pt1
    = pt1 `simf` pt2
headLeafSimMS sf pt1 pt2 = False

headSilentMergeMM :: (Eq a, Ord a) => PMerge a -> PMerge a
headSilentMergeMM mergeF pt1 (NodeN Seq (pt2:ptl2) w2) 
    = seqP [mergeF pt1 pt2, choiceP [Silent w1, seqP ptl2 w2] nw] nw
    where w1 = weight pt1
          nw = w1+w2
headSilentMergeMM mergeF (NodeN Seq (pt2:ptl2) w2) pt1
    = seqP [mergeF pt1 pt2, choiceP [Silent w1, seqP ptl2 w2] nw] nw
    where w1 = weight pt1
          nw = w1+w2


choiceSkipPrefix :: (Eq a, Ord a) => PRule a
choiceSkipPrefix = 
    choiceChildMR (anyMerge (headLeafSimMS (=~=)) (headSilentMergeMM merge))

choiceSkipPrefixCompress :: (Eq a, Ord a) => PRule a
choiceSkipPrefixCompress pt = norm $ choiceFoldPrefix $ choiceSkipPrefix pt


loopChoiceSkip :: (Eq a, Ord a) => PRule a
loopChoiceSkip = 
    choiceChildMR (anyMerge (headLeafSimMS (=&=)) (headSilentMergeMM lmerge))

loopChoiceSkipCompress :: (Eq a, Ord a) => PRule a
loopChoiceSkipCompress pt = norm $ loopChoiceFoldPrefix $ loopChoiceSkip pt

-- concFromChoice (Prefix) 
-- len == 2 only
concFromChoiceMR :: (Eq a, Ord a) => 
    PSim a -> PMerge a -> PRule a
concFromChoiceMR simF mergeF (NodeN Choice ptl w)
    | ptl /= cr  = choiceP cr w
    where cr           = ptlg ++ ptlnf
          (ptlf,ptlnf) = partition isNontrivSeq ptl
          ptlg         = anyMerge simF mergeF ptlf
concFromChoiceMR simF mergeF x = x

concFromChoice :: (Eq a, Ord a) => PRule a
concFromChoice = concFromChoiceMR (conc2SimMS (=~=)) (conc2MergeMM merge)


conc2SimMS :: (Eq a, Ord a) => PSim a -> PSim a
conc2SimMS simF (NodeN Seq (ptx1:pty1:ptl1) w1) (NodeN Seq (pty2:ptx2:ptl2) w2) 
    = ptx1 `simF` ptx2 && pty1 `simF` pty2 
conc2SimMS simF pt1 pt2 = False

conc2MergeMM :: (Ord a) => PMerge a -> PMerge a
conc2MergeMM mergeF (NodeN Seq (ptx1:pty1:ptl1) w1) 
                    (NodeN Seq (pty2:ptx2:ptl2) w2) 
    | null ptl1 && null ptl2     = hc
    | null ptl1 && not (null ptl2)
        = seqP  [hc, choiceP [Silent w1,seqP ptl2 w2] nw ] nw
    | not (null ptl1) && null ptl2 
        = seqP  [hc, choiceP [seqP ptl1 w1,Silent w2] nw]  nw
    | otherwise 
        = seqP [hc, choiceP [ seqP ptl1 w1, seqP ptl2 w2 ] nw ] nw
    where nw = w1+w2
          hc = concP [scale (mergeF ptx1 ptx2) (w1/nw),
                      scale (mergeF pty1 pty2) (w2/nw)] nw



-- concFromChoiceSuffix
-- len == 2 only
concFromChoiceSuffix :: (Eq a, Ord a) => PRule a
concFromChoiceSuffix = concFromChoiceMR conc2TailSim conc2TailMerge 

-- O(n)
conc2TailSim :: (Eq a, Ord a) => PSim a
conc2TailSim (NodeN Seq ptl1 w1) (NodeN Seq ptl2 w2) 
    = length ptl1 > 1 && length ptl2 > 1 
        &&  ptx1 =~= ptx2 && pty1 =~= pty2 
    where (_,[ptx1,pty1]) = tail2 ptl1
          (_,[pty2,ptx2]) = tail2 ptl2
conc2TailSim pt1 pt2 = False

conc2TailMerge :: (Ord a) => PMerge a
conc2TailMerge (NodeN Seq ptl1 w1) 
               (NodeN Seq ptl2 w2) 
    | null hds1 && null hds2     = tc
    | null hds1 && not (null hds2)
        = seqP  [choiceP [Silent w1, seqP hds2 w2] nw, tc] nw
    | not (null hds1) && null hds2 
        = seqP  [choiceP [seqP hds1 w1, Silent w2] nw, tc] nw
    | otherwise 
        = seqP  [choiceP [ seqP hds1 w1, seqP hds2 w2 ] nw, tc] nw
    where nw = w1+w2
          (hds1,[ptx1,pty1]) = tail2 ptl1
          (hds2,[pty2,ptx2]) = tail2 ptl2
          tc = concP [scale (merge ptx1 ptx2) (w1/nw),
                      scale (merge pty1 pty2) (w2/nw)] nw



tail2 :: [a] -> ([a],[a])
tail2 (x:y:xs)  | null xs    = ([],[x,y])
                | otherwise  = (x:nh,nt) 
    where (nh,nt) = tail2 (y:xs)
tail2 [x]   = ([],[])
tail2 []    = ([],[])



lconc2Sim :: (Eq a, Ord a) => PSim a 
lconc2Sim  (NodeN Seq (ptx1:pty1:ptl1) w1) (NodeN Seq (pty2:ptx2:ptl2) w2) 
    =  (ptx1 =~= ptx2  && pty1 =&= pty2)
    || (ptx1 =&= ptx2  && pty1 =~= pty2)
lconc2Sim pt1 pt2 = False

lconc2Merge :: (Ord a) => PMerge a 
lconc2Merge (NodeN Seq (ptx1:pty1:ptl1) w1) 
            (NodeN Seq (pty2:ptx2:ptl2) w2) 
    | null ptl1 && null ptl2     = hc
    | null ptl1 && not (null ptl2)
        = seqP  [hc, choiceP [Silent w1,seqP ptl2 w2] nw ] nw
    | not (null ptl1) && null ptl2 
        = seqP  [hc, choiceP [seqP ptl1 w1,Silent w2] nw]  nw
    | otherwise 
        = seqP [hc, choiceP [ seqP ptl1 w1, seqP ptl2 w2 ] nw ] nw
    where nw = w1+w2
          hc = concP [scale (concPairLoopMerge ptx1 ptx2) (w1/nw),
                      scale (concPairLoopMerge pty1 pty2) (w2/nw)] nw


concPairLoopMerge :: (Eq a, Ord a) => PMerge a
concPairLoopMerge (Node1 PLoop ptx1 r1 w1) (Node1 PLoop ptx2 r2 w2)
    = merge (Node1 PLoop ptx1 r1 w1) (Node1 PLoop ptx2 r2 w2)
concPairLoopMerge (Node1 PLoop ptx1 r1 w1) ptx2 
    = lmerge (Node1 PLoop ptx1 r1 w1) ptx2 
concPairLoopMerge ptx2 (Node1 PLoop ptx1 r1 w1) 
    = lmerge (Node1 PLoop ptx1 r1 w1) ptx2 
concPairLoopMerge ptx1 ptx2 = merge ptx1 ptx2


-- len == 2 only
loopConcFromChoice :: (Eq a, Ord a) => PRule a
loopConcFromChoice = concFromChoiceMR lconc2Sim lconc2Merge 

-- O(n)
lconc2TailSim :: (Eq a, Ord a) => PSim a
lconc2TailSim (NodeN Seq ptl1 w1) (NodeN Seq ptl2 w2) 
    = length ptl1 > 1 && length ptl2 > 1 
        && ( (ptx1 =~= ptx2 && pty1 =&= pty2) 
             ||  (ptx1 =&= ptx2 && pty1 =~= pty2 ) )
    where (_,[ptx1,pty1]) = tail2 ptl1
          (_,[pty2,ptx2]) = tail2 ptl2
lconc2TailSim pt1 pt2 = False

lconc2TailMerge :: (Ord a) => PMerge a
lconc2TailMerge (NodeN Seq ptl1 w1) 
               (NodeN Seq ptl2 w2) 
    | null hds1 && null hds2     = tc
    | null hds1 && not (null hds2)
        = seqP  [choiceP [Silent w1, seqP hds2 w2] nw, tc] nw
    | not (null hds1) && null hds2 
        = seqP  [choiceP [seqP hds1 w1, Silent w2] nw, tc] nw
    | otherwise 
        = seqP  [choiceP [ seqP hds1 w1, seqP hds2 w2 ] nw, tc] nw
    where nw = w1+w2
          (hds1,[ptx1,pty1]) = tail2 ptl1
          (hds2,[pty2,ptx2]) = tail2 ptl2
          tc = concP [scale (concPairLoopMerge ptx1 ptx2) (w1/nw),
                      scale (concPairLoopMerge pty1 pty2) (w2/nw)] nw



loopConcFromChoiceSuffix :: (Eq a, Ord a) => PRule a
loopConcFromChoiceSuffix = concFromChoiceMR lconc2TailSim lconc2TailMerge 


isConc :: PPTree a -> Bool
isConc (NodeN Conc ptl w) = True
isConc pt                 = False

cmpListBy :: (a->a->Bool) -> [a] -> [a] -> Bool
cmpListBy cf (h1:l1) (h2:l2) = h1 `cf` h2 && cmpListBy cf l1 l2
cmpListBy cf l1 l2 = null l1 && null l2 

concSubsumeMR :: (Eq a, Ord a) => LRule a -> PRule a
concSubsumeMR lrule (NodeN Choice ptl w)
    | ptl /= cr     = choiceP cr w
    where cr        = ptlg ++ ptlnf
          (ptlf,ptlnf) = partition (\pt -> isSeq pt || isConc pt) 
                                   ptl
          ptlg         = lrule ptlf
concSubsumeMR lrule x = x

-- pre: order is consistent with similarity
concSeqSim :: (Ord a, Eq a) => PSim a
concSeqSim (NodeN Seq ptl1 w1) (NodeN Conc ptl2 w2) 
    = cmpListBy (=~=) sptl1 sptl2
    where sptl1 = sort (take (length ptl2) ptl1)
          sptl2 = sort ptl2
concSeqSim  (NodeN Conc ptl2 w2) (NodeN Seq ptl1 w1)
    = cmpListBy (=~=) sptl1 sptl2
    where sptl1 = sort (take (length ptl2) ptl1)
          sptl2 = sort ptl2
concSeqSim pt1 pt2 = False

-- seq element comes first
cscaleMerge :: PMerge a -> PMerge a
cscaleMerge mergeF pt1 pt2 
    = scale (mergeF pt1 pt2) (weight pt2/(weight pt1 + weight pt2)) 

-- pre: order is consistent with similarity
concSeqMerge :: (Ord a, Eq a) => PMerge a
concSeqMerge (NodeN Seq (pt1:ptl1) w1) (NodeN Conc ptl2 w2) 
    | null tlptl1 = concres
    | otherwise   = seqP [concres, 
                          choiceP [Silent w2, seqP tlptl1 w1] nw ] nw
    where  (cptl1,tlptl1) = splitAt (length ptl2-1) ptl1
           (nml,ml)       = break (pt1 =~=) ptl2
           (pt2:mltl)     = ml
           sctl           = zipWith (cscaleMerge merge) cptl1 (nml ++ mltl)
           nw             = w1+w2
           concres        = concP (merge pt1 pt2:sctl) nw
concSeqMerge (NodeN Conc ptl2 w2) (NodeN Seq (pt1:ptl1) w1)  
    = concSeqMerge (NodeN Seq (pt1:ptl1) w1) (NodeN Conc ptl2 w2) 
                            

-- full match no child limit
concSubsume :: (Eq a, Ord a) => PRule a
concSubsume = concSubsumeMR (anyMerge concSeqSim concSeqMerge )

lconcSeq2FwdSim :: (Ord a, Eq a) => PSim a 
lconcSeq2FwdSim (NodeN Seq  (ptx1:pty1:ptl1) w1) 
                (NodeN Conc [ptx2,pty2] w2) 
    =  (ptx1 =&= ptx2 && pty1 =~= pty2)
     ||(ptx1 =~= ptx2 && pty1 =&= pty2)
     ||(ptx1 =&= ptx2 && pty1 =&= pty2)
lconcSeq2FwdSim (NodeN Conc  [ptx1,pty1] w1) 
                  (NodeN Seq   (ptx2:pty2:ptl2) w2) 
    = (ptx1 =&= ptx2 && pty1 =~= pty2)
    ||(ptx1 =~= ptx2 && pty1 =&= pty2)
    ||(ptx1 =&= ptx2 && pty1 =&= pty2)
lconcSeq2FwdSim pt1 pt2 = False

lconcSeq2FwdMerge :: (Ord a, Eq a) => PMerge a
lconcSeq2FwdMerge (NodeN Seq  (ptx1:pty1:ptl1) w1) 
                  (NodeN Conc [ptx2,pty2] w2) 
    | null ptl1 = concRes
    | otherwise = seqP [concRes,
                        choiceP [Silent w2, seqP ptl1 w1] nw ] nw
    where nw      = w1+w2
          concRes = concP [concPairLoopMerge ptx1 ptx2, 
                           cscaleMerge concPairLoopMerge pty1 pty2] nw
lconcSeq2FwdMerge  (NodeN Conc [ptx2,pty2] w2) 
                   (NodeN Seq   (ptx1:pty1:ptl1) w1) 
    | null ptl1 = concRes
    | otherwise = seqP [concRes,
                        choiceP [Silent w2, seqP ptl1 w1] nw ] nw
    where nw      = w1+w2
          concRes = concP [concPairLoopMerge ptx1 ptx2, 
                           cscaleMerge concPairLoopMerge pty1 pty2] nw



lconcSeq2RevSim :: (Ord a, Eq a) => PSim a 
lconcSeq2RevSim (NodeN Seq  (ptx1:pty1:ptl1) w1) 
                (NodeN Conc [pty2,ptx2] w2) 
    =  (ptx1 =&= ptx2 && pty1 =~= pty2)
     ||(ptx1 =~= ptx2 && pty1 =&= pty2)
     ||(ptx1 =&= ptx2 && pty1 =&= pty2)
lconcSeq2RevSim (NodeN Conc  [pty1,ptx1] w1) 
                (NodeN Seq   (ptx2:pty2:ptl2) w2) 
    = (ptx1 =&= ptx2 && pty1 =~= pty2)
    ||(ptx1 =~= ptx2 && pty1 =&= pty2)
    ||(ptx1 =&= ptx2 && pty1 =&= pty2)
lconcSeq2RevSim pt1 pt2 = False


lconcSeq2RevMerge :: (Ord a, Eq a) => PMerge a
lconcSeq2RevMerge (NodeN Seq  (ptx1:pty1:ptl1) w1) 
                  (NodeN Conc [pty2,ptx2] w2) 
    | null ptl1 = concRes
    | otherwise = seqP [concRes,
                        choiceP [Silent w2, seqP ptl1 w1] nw ] nw
    where nw      = w1+w2
          concRes = concP [concPairLoopMerge ptx1 ptx2, 
                           cscaleMerge concPairLoopMerge pty1 pty2] nw
lconcSeq2RevMerge  (NodeN Conc [pty2,ptx2] w2) 
                   (NodeN Seq  (ptx1:pty1:ptl1) w1) 
    | null ptl1 = concRes
    | otherwise = seqP [concRes,
                        choiceP [Silent w2, seqP ptl1 w1] nw ] nw
    where nw      = w1+w2
          concRes = concP [concPairLoopMerge ptx1 ptx2, 
                           cscaleMerge concPairLoopMerge pty1 pty2] nw



-- pairs only
loopConcSubsumeFwd :: (Eq a, Ord a) => PRule a
loopConcSubsumeFwd = concSubsumeMR (anyMerge lconcSeq2FwdSim 
                                             lconcSeq2FwdMerge ) 

loopConcSubsumeRev  :: (Eq a, Ord a) => PRule a
loopConcSubsumeRev =  concSubsumeMR (anyMerge lconcSeq2RevSim
                                              lconcSeq2RevMerge ) 

loopConcSubsume  :: (Eq a, Ord a) => PRule a
loopConcSubsume = loopConcSubsumeRev . loopConcSubsumeFwd 

-- choicePrune by a threshold
-- threshold [0,1]
choicePrune :: (Ord a, Eq a) => PPTree a -> Float -> PPTree a
choicePrune (NodeN Choice ptl w) thr
    | null prune  = NodeN Choice ptl w
    | null retain = Silent w
    | otherwise   = choiceP rsretain w
    where (prune,retain)
              = partition (\pt -> weight pt < w*thr) ptl
          wr       = sum $ map weight retain
          rsretain = map (\pt -> scale pt (w/wr)) retain
choicePrune pt thr = pt



-- Rule lists

baseRuleList :: (Eq a, Ord a) => [TRule a]
baseRuleList = [
            TRule{rulename="singleNodeOp",trule=singleNodeOp},
            TRule{rulename="flatten",trule=flattenRule},
            TRule{rulename="fixedLoopRoll", trule=fixedLoopRoll},
            TRule{rulename="silentSeq",trule=silentSeq},
            TRule{rulename="silentConc",trule=silentSeq},
            TRule{rulename="choiceSim",trule=choiceSim},
            TRule{rulename="concSim",trule=concSim},
            TRule{rulename="choiceFoldPrefix",trule=choiceFoldPrefix},
            TRule{rulename="choiceFoldSuffix",trule=choiceFoldSuffix},
            TRule{rulename="choiceSkipPrefix",trule=choiceSkipPrefixCompress},
            TRule{rulename="loopNest",trule=loopNest},
            TRule{rulename="loopGeo",trule=loopGeo},
            TRule{rulename="loopSim",trule=loopSim},
            TRule{rulename="loopChoiceFoldPrefix",trule=loopChoiceFoldPrefix},
            TRule{rulename="loopChoiceFoldSuffix",trule=loopChoiceFoldSuffix},
            TRule{rulename="loopChoiceSkip",trule=loopChoiceSkipCompress},
            TRule{rulename="loopConcSim",trule=loopConcSim},
            TRule{rulename="concFromChoice",trule=concFromChoice},
            TRule{rulename="concFromChoiceSuffix",trule=concFromChoiceSuffix},
            TRule{rulename="concSubsume",trule=concSubsume},
            TRule{rulename="loopConcFromChoice",trule=loopConcFromChoice},
            TRule{rulename="loopConcFromChoiceSuffix",
                  trule=loopConcFromChoiceSuffix},
            TRule{rulename="loopConcSubsume",trule=loopConcSubsume},
            TRule{rulename="loopFixToProb", trule=loopFixToProb} 
            ]


denoiseRuleList :: (Eq a, Ord a) => Float -> [TRule a]
denoiseRuleList thr = 
    baseRuleList ++
    [ TRule{rulename="choicePrune__" ++ show thr, 
      trule=(\pt -> choicePrune pt thr) } ] 

ruleList :: (Show a, Eq a, Ord a) => [TRule a]
ruleList = baseRuleList

-- Rule application

transform :: (Show a, Eq a, Ord a) => PPTree a -> PPTree a
transform = transformRuleOrdered

transformNoise :: (Show a, Eq a, Ord a) => PPTree a -> Float -> PPTree a
transformNoise pt noise = maxTransformRuleOrder pt (denoiseRuleList noise)

-- transformClean x = maxTransformBreadth x ruleList 
transformRuleOrdered :: (Show a, Eq a, Ord a) => PPTree a -> PPTree a
transformRuleOrdered pt = maxTransformRuleOrder pt ruleList

transformInRuleOrder :: (Show a, Eq a) => PPTRuleTransform a
transformInRuleOrder pt [r] = transformPT pt r
transformInRuleOrder pt (r:rs) =
          transformInRuleOrder (transformInRuleOrder pt [r]) rs
transformInRuleOrder x _ = x

maxTransformRuleOrder :: (Show a, Eq a) => PPTRuleTransform a
maxTransformRuleOrder x rules | x == y      = x
                     | otherwise   = maxTransformRuleOrder y rules
                     where y = debug ("=== Count:" ++ show (ncount x)
                                    ++  "===") (transformInRuleOrder x rules)



vrule :: (Show a, Eq a) => PPTree a -> TRule a -> PPTree a
-- validation disabled:
-- vrule x r = trule r x
-- validation enabled:
vrule x r = validateDebug x y r y
     where y = trule r x


transformPT :: (Show a, Eq a) => PPTree a -> TRule a -> PPTree a
transformPT (Node1 op x rp w) rl = vrule (Node1 op (transformPT x rl) rp w) rl
transformPT (NodeN op ptl w) rl =
    vrule (NodeN op (map (`transformPT` rl) ptl) w) rl
transformPT pt rl = vrule pt rl

validateDebug :: (Eq a, Show a) => PPTree a -> PPTree a -> TRule r -> b -> b
validateDebug x y r val
    | x /= y && validate y       = debug msg val
    | x /= y && not (validate y) =
        warn ("*** invalid tree *** "
               ++ valMsg (verboseValidate y)
               ++ " :: " ++ msg) val
    | x == y = val
    -- | x == y = debug (rulename r ++ " 000 " ++ show x) val
    -- super-verbose option
    where msg =  rulename r ++ " " ++ show x ++ " => " ++ show y


