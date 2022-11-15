module OtherRules where

import ProbProcessTree
import Toothpaste

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


