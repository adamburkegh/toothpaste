module Toothpaste where

import ProbProcessTree
import Debug.Trace
import Data.List (sortOn)
import Data.Maybe
import qualified Data.Map as Map

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


-- Rules

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

singleNodeOp :: PRule a
singleNodeOp (NodeN op [u] w)  = u
singleNodeOp x = x

choiceChildMR :: (Eq a, Ord a) => 
    ([PPTree a] -> [PPTree a]) -> PPTree a -> PPTree a
choiceChildMR crule (NodeN Choice ptl w) 
    | ptl /= cr  = choiceP cr w
    where cr = crule ptl
choiceChildMR crule x = x

choiceSim :: (Eq a, Ord a) => PRule a
choiceSim = choiceChildMR choiceSimList

choiceSimList :: (Eq a, Ord a) => LRule a
choiceSimList (u1:u2:ptl) | u1 =~= u2 = choiceSimList (merge u1 u2:ptl)
                          | otherwise = u1 : choiceSimList (u2:ptl)
choiceSimList x = x

loopSim :: (Eq a, Ord a) => PRule a
loopSim = choiceChildMR loopSimList

loopSimList :: (Eq a, Ord a) => LRule a
loopSimList (u1:u2:ptl) | u1 =&= u2 = loopSimList (lmerge u1 u2:ptl)
                          | otherwise = u1 : loopSimList (u2:ptl)
loopSimList x = x



concSim :: Eq a => PRule a
concSim (NodeN Conc ptl w)
    | ptl /= cr = NodeN Conc cr w
    where cr = concSimList ptl
concSim x = x

concSimList :: (Eq a) => LRule a
concSimList (u1:u2:ptl) 
    | u1 =~= u2 = concSimList (Node1 FLoop (merge u1 u2) 2 (w1+w2):ptl)
    | otherwise = u1:concSimList (u2:ptl)
        where w1 = weight u1 
              w2 = weight u2
concSimList x = x

-- This version will identify repeated subsequences of length >1, but has
-- issues. It does not have a formal equivalent in the corresponding papers, 
-- has exponential time and memory complexity, and has a subtle loop bug on some
-- input data.
fixedLoopRollLengthN :: (Eq a, Ord a) => PRule a
fixedLoopRollLengthN (NodeN Seq ptl w) 
    | nptl /= ptl  = seqP nptl w
    where (lss, _) = length ptl `divMod` 2
          rs       = sortOn ncountL (fixedLoopRollForN ptl lss)
          nptl     = head rs
fixedLoopRollLengthN x = x    

-- This version will roll adjacent nodes up, but not recognize repeats of
-- length > 1. It is in the paper
fixedLoopRollSingle :: (Eq a, Ord a) => PRule a
fixedLoopRollSingle (NodeN Seq ((Node1 FLoop u1 r1 w1):uptl) w) 
    | nptl /= ((Node1 FLoop u1 r1 w1):uptl)  = seqP nptl w
    where nptl                               = fixedLoopRollList uptl u1 r1
fixedLoopRollSingle (NodeN Seq (u:uptl) w) 
    | nptl /= (u:uptl)  = seqP nptl w
    where nptl         = fixedLoopRollList uptl u 1
fixedLoopRollSingle x = x    

fixedLoopRoll :: (Eq a, Ord a) => PRule a
fixedLoopRoll = fixedLoopRollSingle


fixedLoopRollList :: (Eq a) => [PPTree a] -> PPTree a -> Float -> [PPTree a]
fixedLoopRollList ((Node1 FLoop u1 r1 w1):ptl) prev ct 
    | u1 == prev = fixedLoopRollList ptl prev (ct+r1-1)
    | u1 /= prev = fixedLoopRollEndPattern prev ct:
                        fixedLoopRollList ptl u1 r1
fixedLoopRollList (u1:ptl) prev ct 
    | u1 == prev = fixedLoopRollList ptl prev (ct+1)
    | u1 /= prev = fixedLoopRollEndPattern prev ct:
                            fixedLoopRollList ptl u1 1
fixedLoopRollList [] prev ct = [fixedLoopRollEndPattern prev ct]

loopRollEndPattern :: (Eq a) => PPTree a -> Float -> POper1 -> PPTree a
loopRollEndPattern prev ct poper
    | ct > 1  = Node1 poper prev ct (weight prev)
    | ct <= 1 = prev

fixedLoopRollEndPattern :: (Eq a) => PPTree a -> Float -> PPTree a
fixedLoopRollEndPattern prev ct = loopRollEndPattern prev ct FLoop 

ncountL :: [PPTree a] -> Int
ncountL ptl = sum $ map ncount ptl

fixedLoopRollForN :: (Eq a) => [PPTree a] -> Int -> [[PPTree a]]
fixedLoopRollForN iptl ls 
    | ls >= 1 && length ptl >= ls 
            = [fixedLoopRollListN ptl ss 1]
                ++ map ([pth] ++) (fixedLoopRollForN (tail iptl) ls) 
                ++ fixedLoopRollForN iptl (ls-1) 
                ++ [existingLoopRoll iptl]
    | ls >= 1 && length ptl < ls = [iptl]
    | ls == 0 = []
    where (ss, ptl) = splitAt ls iptl
          pth = head iptl

-- length == 1
existingLoopRoll :: (Eq a) => [PPTree a] -> [PPTree a]
existingLoopRoll ((Node1 FLoop u1 r1 w1):(Node1 FLoop u2 r2 w2):ptl)
    | u1 == u2 = existingLoopRoll (Node1 FLoop u1 (r1+r2) w1:ptl)
existingLoopRoll ((Node1 FLoop u1 r1 w1):u2:ptl)
    | u1 == u2 = existingLoopRoll (Node1 FLoop u1 (r1+1) w1 :ptl)
existingLoopRoll (u1:(Node1 FLoop u2 r2 w2):ptl)
    | u1 == u2 = existingLoopRoll (Node1 FLoop u1 (r2+1) w2: ptl)
existingLoopRoll (u1:ptl) = u1:existingLoopRoll ptl
existingLoopRoll []       = []

fixedLoopRollListN :: (Eq a) => [PPTree a] -> [PPTree a] -> Float -> [PPTree a]
fixedLoopRollListN iptl prev ct 
    | length iptl < length prev 
        = fixedLoopRollEndPatternL prev ct ++ iptl
    | length iptl >= length prev && next == prev 
        = fixedLoopRollListN ptl prev (ct+1)
    | length iptl >= length prev && next /= prev 
        = fixedLoopRollEndPatternL prev ct ++
                            fixedLoopRollListN ptl next 1
    where (next, ptl) = splitAt (length prev) iptl



loopRollEndPatternL :: (Eq a) => [PPTree a] -> Float -> POper1 -> [PPTree a]
loopRollEndPatternL prev ct poper
    | ct > 1  = [Node1 poper (seqP prev w) ct w]
    | ct <= 1 = prev
    where w = weight $ head prev

fixedLoopRollEndPatternL :: (Eq a) => [PPTree a] -> Float -> [PPTree a]
fixedLoopRollEndPatternL prev ct = loopRollEndPatternL prev ct FLoop 

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
    | otherwise = u1 : loopGeoList (u2:ptl)
loopGeoList x = x

choiceRoll :: (Eq a, Ord a) => PRule a
choiceRoll = choiceChildMR choiceRollList

-- choice roll is a handcrafted loop sim version of choiceSim it seems
-- so redundant with new loopSim rule
-- also buggy sometimes!
choiceRollList :: (Eq a) => LRule a
choiceRollList (u1:(Node1 PLoop u2 r2 w2):ptl) 
    | u1 =~= u2 = choiceRollList  (
                    Node1 PLoop (merge u1 u2) 
                                 ((w1+(r2*w2))/(w1+w2)) 
                                 (w1+w2) 
                    :ptl)
    | otherwise = u1 : choiceRollList (u2:ptl)
                    where w1 = weight u1
choiceRollList ((Node1 PLoop u1 r1 w1):u2:ptl) 
    | u1 =~= u2 = choiceRollList  (
                    Node1 PLoop (merge u1 u2) 
                                 (((r1*w1)+(w2))/(w1+w2)) 
                                 (w1+w2) 
                    :ptl)
    | otherwise = (Node1 PLoop u1 r1 w1) : choiceRollList (u2:ptl)
                    where w2 = weight u2
choiceRollList x = x



flattenRule :: (Eq a) => PRule a
flattenRule x = flatten x

-- choice folds
seqPrefixMerge :: (Eq a, Ord a) => [PPTree a] -> [PPTree a]
seqPrefixMerge ((NodeN Seq (pt1:ptl1) w1):(NodeN Seq (pt2:ptl2) w2):ptl)
    | pt1 =~= pt2 && (ptl1 /= [] || ptl2 /= [])
          = seqP [merge pt1 pt2,
                  choiceP [seqP ptl1 w1,
                           seqP ptl2 w2] nw] nw:
                     seqPrefixMerge ptl
    | pt1 =~= pt2 && (null ptl1 && null ptl2) 
         = merge pt1 pt2:seqPrefixMerge ptl
    | otherwise = NodeN Seq (pt1:ptl1) w1:
                    seqPrefixMerge (NodeN Seq (pt2:ptl2) w2:ptl)
    where nw = w1+w2
seqPrefixMerge ptl = ptl

choiceFoldPrefix :: (Eq a, Ord a) => PRule a
choiceFoldPrefix = choiceChildMR seqPrefixMerge 

-- Warning last is O(N) on lists
seqSuffixMerge :: (Eq a, Ord a) => [PPTree a] -> [PPTree a]
seqSuffixMerge ((NodeN Seq ptl1 w1):(NodeN Seq ptl2 w2):ptl)
    | pt1 =~= pt2 = NodeN Seq [ choiceP [NodeN Seq nptl1 w1,
                                         NodeN Seq nptl2 w2] nw,
                                 merge pt1 pt2] nw:
                     seqSuffixMerge ptl
    | otherwise = NodeN Seq ptl1 w1:
                  seqSuffixMerge (NodeN Seq ptl2 w2:ptl)
     where pt1 = last ptl1
           pt2 = last ptl2
           nptl1 = take (length ptl1-1) ptl1
           nptl2 = take (length ptl2-1) ptl2
           nw = w1+w2
seqSuffixMerge ptl = ptl

-- duplication across prefix suffix folds and maybe other choice
choiceFoldSuffix :: (Eq a, Ord a) => PRule a
choiceFoldSuffix = choiceChildMR seqSuffixMerge

-- choice skips
choiceSkipPrefixMerge :: (Eq a, Ord a) => [PPTree a] -> [PPTree a]
choiceSkipPrefixMerge (pt1:(NodeN Seq (pt2:ptl2) w2):ptl) 
    | pt1 =~= pt2  
        = seqP [merge pt1 pt2, choiceP (silentpt1:ptl2)
                                       (w1+w2) ] 
               (w1+w2):
          choiceSkipPrefixMerge ptl
    | pt1 =~= pt2 && (null ptl2) 
        = merge pt1 pt2:choiceSkipPrefixMerge ptl
    | otherwise 
        = pt1: choiceSkipPrefixMerge (NodeN Seq (pt2:ptl2) w2:ptl)
      where w1      = weight pt1
            silentpt1 = Silent (weight pt1)
choiceSkipPrefixMerge ((NodeN Seq (pt2:ptl2) w2):pt1:ptl)
    | pt1 =~= pt2  
        = seqP [merge pt1 pt2, choiceP (silentpt1:ptl2)
                                       (w1+w2) ] 
               (w1+w2):
          choiceSkipPrefixMerge ptl
    | pt1 =~= pt2 && (null ptl2) 
        = merge pt1 pt2:choiceSkipPrefixMerge ptl
    | otherwise 
        = (NodeN Seq (pt2:ptl2) w2): choiceSkipPrefixMerge (pt1:ptl)
      where w1      = weight pt1
            silentpt1 = Silent (weight pt1)
choiceSkipPrefixMerge ptl = ptl

choiceSkipPrefix :: (Eq a, Ord a) => PRule a
choiceSkipPrefix = choiceChildMR choiceSkipPrefixMerge

choiceSkipPrefixCompress :: (Eq a, Ord a) => PRule a
choiceSkipPrefixCompress pt = norm $ choiceFoldPrefix $ choiceSkipPrefix pt

-- Warning last is O(N) on lists
choiceSkipSuffixMerge :: (Eq a, Ord a) => [PPTree a] -> [PPTree a]
choiceSkipSuffixMerge (pt1:(NodeN Seq (ptl2) w2):ptl) 
    | pt1 =~= pt2  
        = seqP [choiceP (silentpt1:nptl2)
                         nw,
                merge pt1 pt2] 
                nw:
          choiceSkipSuffixMerge ptl
    | otherwise 
        = pt1: choiceSkipSuffixMerge (NodeN Seq (ptl2) w2:ptl)
     where silentpt1 = Silent w1
           pt2 = last ptl2
           nptl2 = take (length ptl2-1) ptl2
           w1 = weight pt1
           nw = w1+w2
choiceSkipSuffixMerge ((NodeN Seq (ptl2) w2):pt1:ptl) 
    | pt1 =~= pt2  
        = seqP [choiceP (silentpt1:nptl2)
                         nw,
                merge pt1 pt2] 
                nw:
          choiceSkipSuffixMerge ptl
    | otherwise 
        =  NodeN Seq (ptl2) w2:choiceSkipSuffixMerge (pt1:ptl)
     where silentpt1 = Silent w1
           pt2 = last ptl2
           nptl2 = take (length ptl2-1) ptl2
           w1 = weight pt1
           nw = w1+w2
choiceSkipSuffixMerge ptl = ptl

choiceSkipSuffix :: (Eq a, Ord a) => PRule a
choiceSkipSuffix = choiceChildMR choiceSkipSuffixMerge

choiceSkipSuffixCompress :: (Eq a, Ord a) => PRule a
choiceSkipSuffixCompress pt = norm $ choiceFoldSuffix $ choiceSkipSuffix pt


-- conc creation
-- len == 2 only
-- includes concSubsume rule as considers all sequence prefixes
concFromChoice :: (Eq a, Ord a) => PRule a
concFromChoice = choiceChildMR concFromChoiceList 

isNontrivSeq :: PPTree a -> Bool
isNontrivSeq (NodeN Seq pt w) = length pt > 1
isNontrivSeq x                = False

concFromChoiceList :: (Eq a, Ord a) => LRule a
concFromChoiceList ptl
    | length rs /= length sq  = rs ++ fl
    | otherwise               = ptl
    where sq = filter isNontrivSeq ptl
          fl = filter (not . isNontrivSeq) ptl
          rs = concFromSeqList sq

concFromSeqList :: (Ord a) => [PPTree a] -> [PPTree a]
concFromSeqList ptl 
    | length rs /= length hds 
        = map (uncurry convertConcMapEntryToNode) (Map.toList rs)
    | otherwise               = ptl
    where (hds,tls) = unzip $ map (splitAt 2 . children) ptl
          rs   = concMapFromSeqChildren hds tls

convertConcMapEntryToNode :: (Ord a) => [PPTree a] -> [[PPTree a]] -> PPTree a
convertConcMapEntryToNode ptl tptl
    | length tptl > 1  && not (null cc) = seqP [concP ptl w, choiceP cc w] w
    | length tptl > 1  && null cc       = concP ptl w
    | otherwise        = seqP (ptl ++ head tptl ) w
    where w  = sum $ map weight ptl
          cc = mapMaybe convertConcTailEntry tptl

convertConcTailEntry :: [PPTree a] -> Maybe (PPTree a)
convertConcTailEntry [pt] = Just pt
convertConcTailEntry []   = Nothing
convertConcTailEntry ptl  = Just (NodeN Seq ptl (weight $ head ptl))

-- pre: sublists of len 2
concMapFromSeqChildren :: (Ord a) => [[PPTree a]] -> [[PPTree a]]
                                        -> Map.Map [PPTree a] [[PPTree a]]
concMapFromSeqChildren (ptl:ptls) (tptl:tptls)
    | ptl `Map.member` sm = Map.update (\x -> Just (tptl:x) ) ptl sm
    | pml `Map.member` sm = Map.update (\x -> Just (tptl:x) ) pml sm
    | otherwise            = Map.insert ptl [tptl] sm
    where sm  = concMapFromSeqChildren ptls tptls
          pml = reverse ptl
concMapFromSeqChildren [] _ = Map.empty



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
            TRule{rulename="choiceSkipSuffix",trule=choiceSkipSuffixCompress},
            TRule{rulename="loopNest",trule=loopNest},
            TRule{rulename="loopGeo",trule=loopGeo},
            TRule{rulename="choiceRoll",trule=choiceRoll},
            TRule{rulename="concFromChoice",trule=concFromChoice},
            TRule{rulename="loopFixToProb", trule=loopFixToProb},
            TRule{rulename="probLoopRoll", trule=loopFixToProb}
            ]

ruleList :: (Show a, Eq a, Ord a) => [TRule a]
ruleList = baseRuleList

-- Rule application

transform :: (Show a, Eq a, Ord a) => PPTree a -> PPTree a
transform = transformRuleOrdered

-- transformClean x = maxTransformBreadth x ruleList 
transformRuleOrdered x = maxTransformRuleOrder x ruleList

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
-- vrule x r = trule r x
-- validation disabled
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


