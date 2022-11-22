module OtherRules where

import ProbProcessTree
import Toothpaste

import Data.List (partition,sortOn)

-- This rule is not a beta trap, ie it may introduce non-determinisim, 
-- so is not included in Toothpaste.

-- Warning last is O(N) on lists
choiceSkipSuffixMerge :: (Eq a, Ord a) => [PPTree a] -> [PPTree a]
choiceSkipSuffixMerge (pt1:(NodeN Seq ptl2 w2):ptl)
    | pt1 =~= pt2
        = seqP [choiceP (silentpt1:nptl2)
                         nw,
                merge pt1 pt2]
                nw:
          choiceSkipSuffixMerge ptl
    | otherwise
        = pt1: choiceSkipSuffixMerge (NodeN Seq ptl2 w2:ptl)
     where silentpt1 = Silent w1
           pt2 = last ptl2
           nptl2 = take (length ptl2-1) ptl2
           w1 = weight pt1
           nw = w1+w2
     -- hlint suggests the above where clause is redundant, but 
     -- I'm not seeing how to factor it out right now
choiceSkipSuffixMerge ((NodeN Seq ptl2 w2):pt1:ptl)
    | pt1 =~= pt2
        = seqP [choiceP (silentpt1:nptl2)
                         nw,
                merge pt1 pt2]
                nw:
          choiceSkipSuffixMerge ptl
    | otherwise
        =  NodeN Seq ptl2 w2:choiceSkipSuffixMerge (pt1:ptl)
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

{-
loopRollEndPattern :: (Eq a) => PPTree a -> Float -> POper1 -> PPTree a
loopRollEndPattern prev ct poper
    | ct > 1  = Node1 poper prev ct (weight prev)
    | ct <= 1 = prev
-}

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

