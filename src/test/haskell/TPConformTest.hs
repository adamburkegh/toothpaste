{-# LANGUAGE ImplicitParams #-}

module TPConformTest where

import qualified Data.Set as Set

import ProbProcessTree
import TPConform

import Test.HUnit
import Test.HUnit.Approx

la  = Leaf "a" 1
la2 = Leaf "a" 2
lb  = Leaf "b" 1
lb2  = Leaf "b" 2
lc  = Leaf "c" 1

-- Tests

probBasicTests =  
             [ "noMatch"    ~: 0 ~=? prob ["c"] la,
               "leafMatch"  ~: 1 ~=? prob ["a"] la,
               "basicChoice" ~: 0.5 ~=? 
                    prob ["a"] (NodeN Choice [la,Silent 1] 2),
               "seq1" ~: 1 ~=? prob ["a"] (NodeN Seq [la] 1),
               "seq3" ~: 1 ~=? prob ["a","b","c"] (NodeN Seq [la,lb,lc] 1),
               "fixedLoopNone" ~: 0 ~=? prob ["a"] (Node1 FLoop lb 5 1),
               "fixedLoop" ~: 1 ~=? prob ["a","a","a"] (Node1 FLoop la 3 1)] 
               
probBasicConcTests = [
               "conc"    ~: 1/2 ~=? prob ["a","b"] (NodeN Conc [la,lb] 2),
               "conc2"   ~: 1/3 ~=? prob ["b","a"] (NodeN Conc [la2,lb] 3),
               "conc3"   ~: 2/3 ~=? prob ["a","b"] (NodeN Conc [la2,lb] 3)]


concTests = [ "conc3a" ~: 1/6 ~=? prob ["a","b","c"] (NodeN Conc [la,lb,lc] 3),
              "conc3b" ~: 1/6 ~=? prob ["a","b","c"] (NodeN Conc [la,lb2,lc] 4),
              "conc3c" ~: 1/4 ~=? prob ["b","a","c"] (NodeN Conc [la,lb2,lc] 4),
              "conc3d" ~: 1/12 
                ~=? prob ["c","a","b"] (NodeN Conc [la,lb2,lc] 4) ] 

cSeq = NodeN Conc [ NodeN Seq [la,lb] 1,
                    lc ] 2
concCompoundTests = [ 
            "compSeq1" ~: 1/4 ~=? prob ["a","b","c"] cSeq,
            "compSeq2" ~: 1/2 ~=? prob ["c","a","b"] cSeq,
            "compSeqInter" ~: 1/4 ~=? prob ["a","c","b"] cSeq,
            "compSeqInvalidOrder1" ~: 0 ~=? prob ["b","a","c"] cSeq ,
            "compSeqInvalidOrder2" ~: 0 ~=? prob ["b","c","a"] cSeq ,
            "compSeqInvalidOrder3" ~: 0 ~=? prob ["c","b","a"] cSeq ,
            "compSeqInvalidDupe" ~: 0 ~=? prob ["a","b","c","c"] cSeq ]  


probDuplicateTests = [ "incomplete" ~: 0 ~=? 1 ]

elemTests = ["elemCompl" ~: [(1,[2,3]),(2,[1,3]),(3,[1,2])] 
                                ~=? elemCompl [1,2,3]    ]

permuteTests =  ["pempty" ~: [[]] ~=? permute ([] :: [Integer]),
                "p1"     ~: [[1]] ~=? permute [1],
                "p2"     ~: [[1,2],[2,1]] ~=? permute [1,2],
                "p3"     ~: [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]
                            ~=? permute [1,2,3] ]

ordSubProjTests = ["empty" ~: ([]::[[Integer]]) ~=? ordSubProj ([]::[Integer]),
                   "one"   ~: [[1]]   ~=? ordSubProj [1],
                   "two"   ~: Set.fromList [[1,2],[1]]   
                                ~=?  Set.fromList (ordSubProj [1,2]),
                   "three"   ~: Set.fromList [[1,2,3],[1,2],[1,3],[1]]   
                                ~=?  Set.fromList (ordSubProj [1,2,3])
                  ] 

ordSubProjPairsTests = 
                  ["empty" ~: ([]::[([Integer],[Integer]) ] ) 
                                    ~=? ordSubProjPairs ([]::[Integer]),
                   "one"   ~: Set.fromList [([1],[]),([],[1])]   
                                ~=? Set.fromList (ordSubProjPairs [1]),
                   "two"   ~: Set.fromList [([1,2],[]),([1],[2]),([],[1,2])]   
                                ~=?  Set.fromList (ordSubProjPairs [1,2]),
                   "three"   ~: Set.fromList [([1,2,3],[]),
                                              ([1,2],[3]),
                                              ([1,3],[2]),
                                              ([1],[2,3]),
                                              ([],[1,2,3])]   
                                ~=?  Set.fromList (ordSubProjPairs [1,2,3])
                  ] 

lfatau2 = Node1 FLoop (NodeN Choice [la2,Silent 1] 3) 2 3
lfatau3 = Node1 FLoop (NodeN Choice [la2,Silent 1] 3) 3 3

fixedLoopTests = let ?epsilon = 0.0001 in 
             [ "loopMatch0_2"  ~: (1/9) ~?~ prob [] lfatau2,
               "loopMatch1_2"  ~: (4/9) ~?~ prob ["a"] lfatau2,
               "loopMatch2_2"  ~: (4/9) ~?~ prob ["a","a"] lfatau2,
               "loopMatch2_2"  ~: 0 ~=? prob ["a","a","a"] lfatau2,
               "loopMatch0_3"  ~: (1/27) ~?~ prob [] lfatau3,
               "loopMatch1_3"  ~: (6/27) ~?~ prob ["a"] lfatau3,
               "loopMatch2_3"  ~: (12/27) ~?~ prob ["a","a"] lfatau3,
               "loopMatch3_3"  ~: (8/27) ~?~ prob ["a","a","a"] lfatau3,
               "loopMatch2_4"  ~: 0 ~=? prob ["a","a","a","a"] lfatau3 ]


lpa = Node1 PLoop la 3 1
lpatau = Node1 PLoop (NodeN Choice [la2,Silent 1] 3) 4 3

probLoopTests =  [ "noMatch"    ~: 0 ~=? prob ["c"] lpa ] --,
               --"loopMatch"  ~: (2/9) ~=? prob ["a"] lpa,
               -- "loopMatch2"  ~: (2**3/3**4) ~=? prob ["a","a","a"] lpa,
               -- "silentChoiceLoop"  ~: 2/(4**2)  ~=? prob ["a"] lpatau ,
               -- "silentChoiceLoop2"  ~: 1/3 ~=? prob [] lpatau ]

utilTests = elemTests ++ permuteTests ++ ordSubProjTests 
            ++ ordSubProjPairsTests

probTests = probBasicTests ++ probLoopTests ++ fixedLoopTests 
            -- ++ probBasicConcTests ++ concTests ++ concCompoundTests

huTests = probTests ++ utilTests

