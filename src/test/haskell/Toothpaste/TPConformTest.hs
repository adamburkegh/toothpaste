{-# LANGUAGE ImplicitParams #-}

module Toothpaste.TPConformTest where

import qualified Data.Set as Set

import Toothpaste.ProbProcessTree
import Toothpaste.TPConform
import Toothpaste.WeightedAutomata

-- import PPTTestUtil

import Test.HUnit
import Test.HUnit.Approx

la  = Leaf "a" 1
la2 = Leaf "a" 2
la3 = Leaf "a" 3
la4 = Leaf "a" 4
la10 = Leaf "a" 10
lb  = Leaf "b" 1
lb2 = Leaf "b" 2
lb4 = Leaf "b" 4
lc  = Leaf "c" 1
lc2 = Leaf "c" 2
lc4 = Leaf "c" 4
lc8 = Leaf "c" 8
ld  = Leaf "d" 1
ld2 = Leaf "d" 2
ld4 = Leaf "d" 4
le2 = Leaf "e" 2
le  = Leaf "e" 1
le4 = Leaf "e" 4
le8 = Leaf "e" 8
lf  = Leaf "f" 1
lf2 = Leaf "f" 2
lf4 = Leaf "f" 4

eps =  0.0001


tokprob :: String -> PPTree String -> Float
tokprob s = prob (map (: []) s) 

pftokprob :: String -> PFTree String -> Float
pftokprob s = pfprob (map (: [])  s) 

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
               
basicSilentTests = [
            "silentLeafOk" ~: 1 ~=? prob ([]::String) (Silent 1),
            "silentLeafFail" ~: 0 ~=? prob ["a"] (Silent 1),
            "seqSilent" ~: 1.0 ~=?
                   prob ["a","b"] (NodeN Seq [la, Silent 1, lb] 1),
            -- "silentConc" ~: 1.0 ~=? prob ["a"] (NodeN Conc [la,Silent 1] 2),
            "silentChoice" ~: 0.5 ~=? prob ["a"] (NodeN Choice [la,Silent 1] 2),
            "silentChoiceEmpty" ~: 0.5 ~=?
                prob [] (NodeN Choice [la,Silent 1] 2)
                ]


probBasicConcTests = [
               "conc"    ~: 1/2 ~=? prob ["a","b"] (NodeN Conc [la,lb] 2),
               "conc2"   ~: 1/3 ~=? prob ["b","a"] (NodeN Conc [la2,lb] 3),
               "conc3"   ~: 2/3 ~=? prob ["a","b"] (NodeN Conc [la2,lb] 3)]


concSimpleTests = let ?epsilon = eps in
       ["conc3evens1" ~: 1/6 ~=? prob ["a","b","c"] (NodeN Conc [la,lb,lc] 3),
        "conc3evens2" ~: 1/6 ~=? prob ["a","c","b"] (NodeN Conc [la,lb,lc] 3),
        "conc3a1" ~: 1/6  ~?~ prob ["a","b","c"] (NodeN Conc [la,lb2,lc] 4),
        "conc3a2" ~: 1/12 ~?~ prob ["a","c","b"] (NodeN Conc [la,lb2,lc] 4),
        "conc3b1" ~: 1/4  ~?~ prob ["b","a","c"] (NodeN Conc [la,lb2,lc] 4),
        "conc3b2" ~: 1/4  ~?~ prob ["b","c","a"] (NodeN Conc [la,lb2,lc] 4),
        "conc3c1" ~: 1/12 ~?~ prob ["c","a","b"] (NodeN Conc [la,lb2,lc] 4),
        "conc3c2" ~: 1/6  ~?~ prob ["c","b","a"] (NodeN Conc [la,lb2,lc] 4) ]


cSeq = NodeN Conc [ NodeN Seq [la,lb] 1,
                    lc ] 2
concSeqCompound1 = [ 
            "compSeq1" ~: 1/4 ~=? prob ["a","b","c"] cSeq,
            "compSeq2" ~: 1/2 ~=? prob ["c","a","b"] cSeq,
            "compSeqInter" ~: 1/4 ~=? prob ["a","c","b"] cSeq,
            "compSeqInvalidOrder1" ~: 0 ~=? prob ["b","a","c"] cSeq ,
            "compSeqInvalidOrder2" ~: 0 ~=? prob ["b","c","a"] cSeq ,
            "compSeqInvalidOrder3" ~: 0 ~=? prob ["c","b","a"] cSeq ,
            "compSeqInvalidDupe" ~: 0 ~=? prob ["a","b","c","c"] cSeq ]  

cSeq2 = NodeN Conc [ NodeN Seq [la,lb,lc] 1,
                    ld ] 2

concSeqCompound2 = [
            "compSeq1" ~: 1/8 ~=? prob ["a","b","c","d"] cSeq2,
            "compSeq2" ~: 1/2 ~=? prob ["d","a","b","c"] cSeq2,
            "compSeqInter1" ~: 1/4 ~=? prob ["a","d","b","c"] cSeq2,
            "compSeqInter2" ~: 1/8 ~=? prob ["a","b","d","c"] cSeq2,
            "compSeqInvalidOrder1" ~: 0 ~=? prob ["b","a","c","d"] cSeq2 ,
            "compSeqInvalidOrder2" ~: 0 ~=? prob ["b","c","a","d"] cSeq2 ,
            "compSeqInvalidOrder3" ~: 0 ~=? prob ["c","b","a","d"] cSeq2 ,
            "compSeqInvalidDupe" ~: 0 ~=? prob ["a","b","c","c"] cSeq2 ]

concSeqCompound = concSeqCompound1 ++ concSeqCompound2

cChoice = NodeN Conc [ NodeN Choice [la2, Silent 1] 3,
                       lc] 4
concChoiceCompound1 = [
            "compChoice1" ~: 1/2 ~=? prob ["a", "c"] cChoice ,
            "compChoice2" ~: 1/3 ~=? prob ["c"] cChoice ,
            "compChoice3" ~: 1/6 ~=? prob ["c", "a"] cChoice,
            "compChoiceWrong" ~: 0 ~=? prob ["a"] cChoice ]


cChoice2 = NodeN Conc [ NodeN Choice [la2, Silent 1] 3,
                       lc,ld] 5
concChoiceCompound2 = let ?epsilon = eps in [
            "compChoice2-1" ~: 1/5  ~?~ prob ["a","c","d" ] cChoice2,
            "compChoice2-2" ~: 1/5  ~?~ prob ["a","d","c" ] cChoice2,
            "compChoice2-3" ~: 5/30 ~?~ prob ["c","d" ] cChoice2,
            "compChoice2-4" ~: 5/30 ~?~ prob ["d","c" ] cChoice2,
            "compChoice2-5" ~: 1/10 ~?~ prob ["c","a","d"] cChoice2,
            "compChoice2-6" ~: 1/30 ~?~ prob ["c","d","a"] cChoice2,
            "compChoice2-7" ~: 1/10 ~?~ prob ["d","a","c"] cChoice2,
            "compChoice2-8" ~: 1/30 ~?~ prob ["d","c","a"] cChoice2,
            "compChoice2-9" ~: 0.0  ~=? prob ["a","b"] cChoice2 ]

cConc1 = NodeN Conc [la, NodeN Conc [lb,lc4] 5] 6
concConcCompound = let ?epsilon = eps in [
        "concc1" ~: 11/180 ~?~ prob ["a","b","c"] cConc1,
        "concc2" ~: 44/180 ~?~ prob ["a","c","b"] cConc1,
        "concc3" ~: 1/36   ~?~ prob ["b","a","c"] cConc1,
        "concc4" ~: 1/9    ~?~ prob ["b","c","a"] cConc1,
        "concc5" ~: 5/18   ~?~ prob ["c","a","b"] cConc1,
        "concc6" ~: 5/18   ~?~ prob ["c","b","a"] cConc1,
        "conccTotal" ~: 1  
            ~?~ (44/180) + (11/180) +  (1/36) + (1/9) + (5/18) + (5/18)
    ]

concCompoundTests = concSeqCompound
                    ++ concChoiceCompound1 ++ concChoiceCompound2
                    ++ concConcCompound

cSil = NodeN Conc [ NodeN Seq [la,Silent 1,lb] 1,
                    lc ] 2
concCompoundSilentTests = [
            "compSeq1" ~: 1/8 ~=? prob ["a","b","c"] cSil,
            "compSeq2" ~: 1/2 ~=? prob ["c","a","b"] cSil,
            "compSeqInter" ~: 3/8 ~=? prob ["a","c","b"] cSil,
            "compSeqInvalidOrder1" ~: 0 ~=? prob ["b","a","c"] cSil ,
            "compSeqInvalidOrder2" ~: 0 ~=? prob ["b","c","a"] cSil ,
            "compSeqInvalidOrder3" ~: 0 ~=? prob ["c","b","a"] cSil ,
            "compSeqInvalidDupe" ~: 0 ~=? prob ["a","b","c","c"] cSil ]

cLL1 = NodeN Conc [la, Node1 PLoop lb2 (3/2) 2] 3
{-
PPT for cLL1: 
Conc:3.0
  "a":1.0
  PLoop[3/2]:2.0
    "b":2.0

 Petri net (SLPN) for cLL1 
 [tau 3] +- p1 --- [a 1]---------------------------pae-----[tau 3]
         |                                                /
     +- p2 --- [tau 2]---pl----[tau 2*2/3]-----ple---/
                             |  \
                             |    \
                            pl----[b 2*1/3]

 -}
concLoopTests = let ?epsilon = 0.01
                    ?eps     = 0.01
   in [ "concLeafLoop1" ~: 2/3     ~?~ probEps ["a"] cLL1,
        "concLeafLoop2" ~: 10/81   ~?~ probEps ["a","b"] cLL1,
        "concLeafLoop3" ~: 24/243  ~?~ probEps ["b","a"] cLL1
    ]


-- shows Flatten is stochastically lossy for conc
concSingleCollapse = [
    "nestedSingleConc1" ~: 3/4 ~=? tokprob "ab" (concP [ la, concP [lb] 1 ] 2),
    "nestedSingleConc2" ~: 1/4 ~=? tokprob "ba" (concP [ la, concP [lb] 1 ] 2)
    ]

dpab  = NodeN Choice [la,la2,lb] 4
dpabc = NodeN Choice [la2, NodeN Seq [la3,
                                      NodeN Choice [lb,Silent 2] 3] 3, 
                      lc] 4 

probDuplicateTests = [ 
        "leafChoice1" ~: 3/4  ~=? tokprob "a"  dpab,
        "leafChoice2" ~: 1/4  ~=? tokprob "b"  dpab,
        "seqChoice1"  ~: 2/3  ~=? tokprob "a"  dpabc,
        "seqChoice2"  ~: 1/6  ~=? tokprob "ab" dpabc,
        "seqChoice3"  ~: 0    ~=? tokprob "b"  dpabc,
        "seqChoice4"  ~: 1/6  ~=? tokprob "c"  dpabc  ] 

elemTests = ["elemCompl" ~: [(1,[2,3]),(2,[1,3]),(3,[1,2])] 
                                ~=? elemCompl [1,2,3]    ]

permuteTests =  ["pempty" ~: [[]] ~=? permute ([] :: [Integer]),
                "p1"     ~: [[1]] ~=? permute [1],
                "p2"     ~: [[1,2],[2,1]] ~=? permute [1,2],
                "p3"     ~: [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]
                            ~=? permute [1,2,3] ]

lfatau2 = Node1 FLoop (NodeN Choice [la2,Silent 1] 3) 2 3
lfatau3 = Node1 FLoop (NodeN Choice [la2,Silent 1] 3) 3 3

fixedLoopTests = let ?epsilon = eps in 
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
lpaseq = Node1 PLoop (NodeN Seq [la,lb] 1) 3 1
lpach  = Node1 PLoop (NodeN Choice [la3,lb] 4) 3 4
lpach2  = Node1 PLoop (NodeN Choice [la3,NodeN Seq [lb,lc] 1] 4) 3 4
lpatau = Node1 PLoop (NodeN Choice [la2,Silent 1] 3) 3 3

probLoopTests =  let ?epsilon = eps in 
            [ "noMatch"    ~: 0 ~=? prob ["c"] lpa ,
              "emptySingleton" ~: (1/3) ~=? prob [] lpa,
              "loopMatch"  ~: (2/9) ~=? prob ["a"] lpa ,
              "loopMatch2" ~: (2**3/3**4) ~?~ prob ["a","a","a"] lpa ,
              "noMatchSeq" ~: 0 ~=? prob ["a","a"] lpaseq,
              "twoMatchSeq" ~: (2**2/3**3) ~?~ prob ["a","b","a","b"] lpaseq,
              "noMatchChoice" ~: 0 ~=? prob ["c"] lpach,
              "oneMatchChoice" ~: 3*2/(4*3*3) ~?~ prob ["a"] lpach,
              "twoMatchChoice1" ~: 3*2*2/(4*4*3**3) ~?~ prob ["a","b"] lpach,
              "twoMatchChoice2" ~: 3*2*2/(4*4*3**3) ~?~ prob ["b","a"] lpach,
              "threeMatchChoice" ~: 3*3*2**3/(4**3*3**4) ~?~ 
                                        prob ["a","b","a"] lpach,
              "twoMatchChoice3" ~: 3*2*2/(4*4*3**3) ~?~ 
                                            prob ["b","c","a"] lpach2,
              "silentChoiceLoop"  ~: 2*2/(3*9) + 2/(3*3*9)  
                                        ~=? prob ["a"] lpatau,
              "silentChoiceLoopEmpty" ~: 1/3 + 1/(3*3**2) ~=? prob [] lpatau,
              "silentChoiceLoop2" ~: 0.13994 ~?~ prob ["a","a"] lpatau] 

loopApproxKTests = [ "t0" ~: 1  ~=? findLoopApproxK 2 0.6,
                     "t1" ~: 2  ~=? findLoopApproxK 2 0.5,
                     "t2" ~: 2  ~=? findLoopApproxK 2 0.3,
                     "t3" ~: 3  ~=? findLoopApproxK 2 0.2,
                     "t4" ~: 4  ~=? findLoopApproxK 2 0.12,
                     "t5" ~: 2  ~=? findLoopApproxK 3 0.9,
                     "t6" ~: 3  ~=? findLoopApproxK 3 0.6,
                     "t7" ~: 6  ~=? findLoopApproxK 3 0.2,
                     "t7" ~: 14 ~=? findLoopApproxK 3 0.01 ]


choiceLS2 = choiceP [Node1 PLoop la3 2.6 3,
                     seqP [lb,la,ld,le] 1,
                     seqP [lb,lc,ld,le] 1 ] 5

compoundLoopTests1 = let ?epsilon = eps in 
    [
    "c1" ~: tokprob "a"     choiceLS2 ~?~ 0.142,
    "c2" ~: tokprob "aa"    choiceLS2 ~?~ 0.08739,
    "c3" ~: tokprob "aaa"   choiceLS2 ~?~ 0.05378,
    "c4" ~: tokprob "aaaa"  choiceLS2 ~?~ 0.03310,
    "c5" ~: tokprob "bade"  choiceLS2 ~?~ 0.2,
    "c6" ~: tokprob "bcde"  choiceLS2 ~?~ 0.2
    ]



compoundLoopTests = compoundLoopTests1 


loudTests = [ "silent" ~: False ~=? loud (Silent 1),
              "leaf" ~:   True  ~=? loud la,
              "choice" ~: True  ~=? loud (NodeN Choice [la,lb] 2),
              "ploop" ~:  False ~=? loud (Node1 PLoop la 3 1) ]



pathsetPFBasicTests = [
            "silent" ~: PFNode PFSilent [] 3 
                            ~=? ps3 (Silent 3::(PPTree String)),
            "leaf"   ~: PFNode pfa [] 1 ~=? ps3 la,
            "seq"    ~: PFNode pfa [PFNode pfb [] 1] 1
                            ~=?  ps3 (NodeN Seq [la,lb] 1),
            "choice" ~: PFNode PFNull [PFNode pfa [] 1,
                                        PFNode pfb [] 2] 3
                            ~=? ps3 (NodeN Choice [la,lb2] 3), 
            "floop" ~: PFNode pfa [PFNode pfa [PFNode pfa [] 1] 1] 1 
                            ~=? ps3 (Node1 FLoop la 3 1),
            "ploop" ~: PFNode PFSilent [PFNode PFSilent [] (1/3),
                                        PFNode pfa [] (2/(3*3)),
                                        PFNode pfa 
                                                [PFNode pfa [] 1] (2*2/3**3),
                                        PFNode pfa 
                                                [PFNode pfa 
                                                        [PFNode pfa [] 1] 1] 
                                                (2**3/3**4),
                                        PFNode pfa 
                                                [PFNode pfa 
                                                        [PFNode pfa 
                                                                [PFNode pfa 
                                                                        [] 1] 
                                                                1] 1] 
                                                (2**4/3**5)
                                                  ] 1
                            ~=? pathset (Node1 PLoop la 3 1) 0.5, -- k == 4
            "ploop2" ~: PFNode PFSilent [pfsilent (2*2/3),
                                         PFNode pfb [] (2*2/3**2),
                                         PFNode pfb [pflb2] (2*2/3**3)] 2
                            ~=? pathset (Node1 PLoop lb2 (3/2) 2) 0.1 -- k == 2 
                            ]

pathsetSingletons = [
        "conc" ~: PFNode PFSilent 
                          [PFNode pfa 
                                  [PFNode PFSilent [] 1] 1] 1 
                    ~=? pathset (NodeN Conc [la] 1) eps
        ]
               
pathsetPFConcTests = [
        "twoLeaves" ~: 
            PFNode PFSilent [PFNode pfa [PFNode pfb [pfsilent 2] 1] 1,
                             PFNode pfb [PFNode pfa [pfsilent 2] 1] 1] 2
               ~=? pfnorm ( pathset (NodeN Conc [la,lb] 2) eps ),
        "oneLeafOneSilent" ~: 
            PFNode PFSilent [PFNode pfa [PFNode PFSilent [pfsilent 2] 1] 1,
                             PFNode PFSilent [PFNode pfa [pfsilent 2] 1] 1] 2
            ~=? pfnorm (pathset (NodeN Conc [la,Silent 1] 2) eps),
        "twoLeafOneSilent" ~:
            PFNode PFSilent [PFNode pfa 
                                    [PFNode pfb 
                                        [PFNode PFSilent [pfsilent 6] 2] 2,
                                     PFNode PFSilent 
                                        [PFNode pfb [pfsilent 6] 2 ] 2] 2,
                              PFNode pfb 
                                    [PFNode pfa 
                                        [PFNode PFSilent [pfsilent 6] 2] 2,
                                     PFNode PFSilent 
                                        [PFNode pfa [pfsilent 6] 2] 2] 2,
                              PFNode PFSilent 
                                    [PFNode pfa 
                                        [PFNode pfb [pfsilent 6] 2] 2,
                                     PFNode pfb 
                                        [PFNode pfa [pfsilent 6] 2] 2 ] 2 ]
                    6
            ~=? pfnorm (pathset (NodeN Conc [la2,Silent 2, lb2] 6) eps),
        "leafLoop" ~: 
            pfnorm 
                (PFNode PFSilent 
                    [PFNode pfa  
                            [PFNode PFSilent 
                                    [PFNode PFSilent [pfsilent 3] (4/3),
                                     PFNode pfb [pfsilent 3] (2*2/(3*3)),
                                     PFNode pfb 
                                        [PFNode pfb 
                                            [pfsilent 3] 2] (2*2/3**3) ] 2] 1,
                     PFNode PFSilent 
                            [PFNode pfa [PFNode PFSilent [pfsilent 3] (4/3),
                                         PFNode pfb [pfsilent 3] (2*2/(3*3)),
                                         PFNode pfb 
                                            [PFNode pfb 
                                                [pfsilent 3] 2] (2*2/3**3) ] 1,
                             PFNode PFSilent [PFNode pfa [pfsilent 3] 1] (4/3),
                             PFNode pfb [PFNode pfa [pfsilent 3] 1] (2*2/(3*3)),
                             PFNode pfb 
                                [PFNode pfa [PFNode pfb [pfsilent 3] 2] 1,
                                 PFNode pfb 
                                    [PFNode pfa 
                                        [pfsilent 3] 1] 2 ] (2*2/3**3) ] 
                            2] 3) 
            ~=? pfnorm (pathset (NodeN Conc [la,Node1 PLoop lb2 (3/2) 2] 3) 0.1)
    ]

pathsetPFTests = pathsetPFBasicTests ++ pathsetPFConcTests


    


--

cab = choiceP [la,lb] 2
ccd = choiceP [lc,ld] 2

shuffle :: (Eq a, Ord a) => PPTree a -> PPTree a -> PFTree a
shuffle x y = pfshuffle (pathset x eps) (pathset y eps)

cc1 =  shuffle (choiceP [la2, seqP [lb2,lc2] 2] 4)
                            (choiceP [ld2, le2] 4)
shuffleProbChoiceChoiceTests = [ 
    "choiceChoice1p1"  ~: 1/8 ~=?  pftokprob "ad" cc1 ,
    "choiceChoice1p2"  ~: 1/8 ~=?  pftokprob "ae" cc1 ,
    "choiceChoice1p3"  ~: 1/24 ~=? pftokprob "bce" cc1 ,
    "choiceChoice1p4"  ~: 1/24 ~=? pftokprob "bcd" cc1 ,
    "choiceChoice1p5"  ~: 1/12 ~=? pftokprob "bdc" cc1 ,
    "choiceChoice1p6"  ~: 1/12 ~=? pftokprob "bec" cc1 ,
    "choiceChoice1p7"  ~: 1/8 ~=?  pftokprob "da" cc1 ,
    "choiceChoice1p8"  ~: 1/8 ~=?  pftokprob "dbc" cc1 ,
    "choiceChoice1p9"  ~: 1/8 ~=?  pftokprob "ea" cc1,
    "choiceChoice1p10" ~: 1/8 ~=?  pftokprob "ebc" cc1
    ]
    
cl1 = shuffle (choiceP [la2,lb2] 4) lc4
shuffleProbChoiceLeafTests1 = [
    "cl1" ~: 0   ~=? pftokprob "ab" cl1,
    "cl2" ~: 1/4 ~=? pftokprob "ac" cl1,
    "cl3" ~: 1/4 ~=? pftokprob "bc" cl1,
    "cl4" ~: 1/4 ~=? pftokprob "ca" cl1,
    "cl5" ~: 1/4 ~=? pftokprob "cb" cl1
    ]

    
cl2 = shuffle (choiceP [la2,lb2] 4) lc2
shuffleProbChoiceLeafTests2 = [
    "cl1" ~: 0   ~=? pftokprob "ab" cl2,
    "cl2" ~: 1/3 ~=? pftokprob "ac" cl2,
    "cl3" ~: 1/3 ~=? pftokprob "bc" cl2,
    "cl4" ~: 1/6 ~=? pftokprob "ca" cl2,
    "cl5" ~: 1/6 ~=? pftokprob "cb" cl2
    ]

shuffleProbChoiceTermTests = [
        "ct1" ~:  all
                ((\ p -> p == 1 / 8) . (\ s -> pftokprob s (shuffle cab ccd)))
                ["ac", "ad", "bc", "bd", "ca", "cb", "da", "db"]
                    @? "1/8 expected"
    ]

cs1 = shuffle (choiceP [la2, seqP [lb,lc] 1,ld] 4) le4 
shuffleProbChoiceSeqTests = [
    "cs1" ~: 1/4 ~=? pftokprob "ae" cs1,
    "cs2" ~: 1/40 ~=? pftokprob "bce" cs1,
    "cs3" ~: 1/10 ~=? pftokprob "bec" cs1,
    "cs4" ~: 1/8 ~=? pftokprob "de" cs1,
    "cs5" ~: 1/4 ~=? pftokprob "ea" cs1,
    "cs6" ~: 1/8 ~=? pftokprob "ebc" cs1,
    "cs7" ~: 1/8 ~=? pftokprob "ed" cs1
    ] 

ccs1 = shuffle (choiceP [la, seqP [lb,lc] 1] 2)
               (choiceP [ld, seqP [le,lf] 1] 2) 
shuffleProbChoiceChoiceSeqTests = [
    "cs1" ~:  1/8 ~=?  pftokprob "ad"   ccs1,
    "cs2" ~:  1/8 ~=?  pftokprob "aef"  ccs1,
    "cs3" ~:  1/12 ~=? pftokprob "bdc"  ccs1,
    "cs4" ~:  1/24 ~=? pftokprob "becf" ccs1,
    "cs5" ~:  1/24 ~=? pftokprob "bcef" ccs1,
    "cs6" ~:  1/24 ~=? pftokprob "bcd"  ccs1,
    "cs7" ~:  1/8 ~=?  pftokprob "da"   ccs1,
    "cs8" ~:  1/8 ~=?  pftokprob "dbc"  ccs1,
    "cs9" ~:  1/12 ~=? pftokprob "eaf"  ccs1,
    "cs10" ~: 1/24 ~=? pftokprob "efa"  ccs1,
    "cs11" ~: 1/24 ~=? pftokprob "efbc"  ccs1,
    "cs12" ~: 1/24 ~=? pftokprob "ebfc"  ccs1,
    "cs13" ~: 1/24 ~=? pftokprob "ebcf"  ccs1
    ]

shuffleProbTests = shuffleProbChoiceLeafTests1 ++ shuffleProbChoiceLeafTests2 
    ++ shuffleProbChoiceChoiceTests 
    ++ shuffleProbChoiceTermTests
    ++ shuffleProbChoiceSeqTests
    ++ shuffleProbChoiceChoiceSeqTests


--
pfa = PFSymbol "a"
pfb = PFSymbol "b"
pfc = PFSymbol "c"
pfd = PFSymbol "d"
pfe = PFSymbol "e"
pff = PFSymbol "f"



pfProbTests = [ 
    "leaf1"     ~: 1   ~=? pfprob ["a"] (PFNode pfa [] 1),
    "leaf2"     ~: 0   ~=? pfprob ["b"] (PFNode pfa [] 1),
    "silent1"   ~: 1   ~=? pfprob ([]::[String])  (PFNode PFSilent [] 1),
    "silent2"   ~: 0   ~=? pfprob ["b"] (PFNode PFSilent [] 1),
    "null1"   ~: 1   ~=? pfprob ([]::[String])  (PFNode PFNull [] 1),
    "choice1"   ~: 1/2 ~=? pfprob ["a"]
                                  (PFNode pfa 
                                          [PFNode PFSilent [] 1,
                                           PFNode pfb [] 1] 5),
    "choice2"   ~: 1/3 ~=? pfprob ["a"]
                                  (PFNode PFNull 
                                          [PFNode pfa [] 1,
                                           PFNode pfb [] 2] 5),
    "choice2"   ~: 2/3 ~=? pfprob ["a","b"]
                                  (PFNode pfa 
                                          [PFNode PFSilent [] 1,
                                           PFNode pfb [] 2] 5),
    "emptyLeaf" ~: 0   ~=? pfprob ([]::[String]) pfla,
    "emptyChoice" ~: 0   ~=? pfprob ([]::[String]) 
                                    (PFNode pfa
                                          [PFNode PFSilent [] 1,
                                           PFNode pfb [] 1] 5)
    ]


pfla   = PFNode pfa [] 1
pfla2  = PFNode pfa [] 2
pflb   = PFNode pfb [] 1
pflb2  = PFNode pfb [] 2
pflb4  = PFNode pfb [] 4
pflc   = PFNode pfc [] 1
pflc2  = PFNode pfc [] 2
pflc4  = PFNode pfc [] 4
pfld   = PFNode pfd [] 1
pfld2  = PFNode pfd [] 2
pfld4  = PFNode pfd [] 4
pfle2  = PFNode pfe [] 2
pfle4  = PFNode pfe [] 4
pflf   = PFNode pff [] 1

pfSingleShuffleTests =  [
    "leaves" ~: PFNode PFNull [PFNode pfa [pflb] 1,
                                 PFNode pfb [pfla] 1] 2
                ~=? pfshuffle pfla pflb,
    "leavesUneven" ~: PFNode PFNull [PFNode pfa [pflb2] 1,
                                     PFNode pfb [pfla] 2] 3
                ~=? pfshuffle pfla pflb2 ,
    "leafSeq" ~: pfnorm (PFNode PFNull [PFNode pfa [PFNode pfb [pflc] 1] 2,
                                  PFNode pfb [PFNode pfc [pfla2] 1,
                                              PFNode pfa [pflc] 2] 1] 
                                 3)
                ~=? pfnorm (pfshuffle pfla2 (PFNode pfb [pflc] 1) ) ,
    "silentSeq" ~: pfnorm (PFNode PFNull 
                                 [PFNode PFSilent [PFNode pfb [pflc] 1] 2,
                                  PFNode pfb 
                                         [PFNode pfc 
                                                 [PFNode PFSilent [] 2] 1,
                                          PFNode PFSilent [pflc] 2] 1] 
                          3 )
                ~=? pfnorm (pfshuffle (PFNode PFSilent [] 2) 
                                      (PFNode pfb [pflc] 1) ),
    "choiceLeaf" ~: PFNode PFNull
                           [PFNode pfa [pflc4] 2,
                            PFNode pfb [pflc4] 2,
                            PFNode pfc [pfla2,pflb2] 4] 
                            8
                ~=? pfshuffle (PFNode PFNull [pfla2,pflb2] 4) pflc4 ,
    "twoTermChoices" ~: PFNode PFNull
                               [PFNode pfa [pflc,pfld] 1,
                                PFNode pfb [pflc,pfld] 1,
                                PFNode pfc [pfla,pflb] 1,
                                PFNode pfd [pfla,pflb] 1 ] 4
                ~=? pfshuffle (PFNode PFNull [pfla,pflb] 2) 
                              (PFNode PFNull [pflc,pfld] 2) 
                ]

pfCollapseTests = [
    "leaf" ~: pfla ~=? pfcollapse pfla,
    "oneNull" ~: PFNode PFNull ([]::[PFTree String]) 1 
        ~=? pfcollapse (PFNode PFNull [] 1),
    "nullParent" ~: pfla ~=? pfcollapse (PFNode PFNull [pfla] 1),
    "nullChild1" ~: PFNode pfa [pfla,pflb] 1
        ~=? pfcollapse (PFNode pfa [PFNode PFNull [pfla,pflb] 1] 1),
    "nullChild2" ~: PFNode PFNull [pfla,pflb,pflc] 1
        ~=? pfcollapse (PFNode PFNull [PFNode PFNull [pfla,pflb,pflc] 1] 1)
    ]


pfShuffleSeqTests = [
    "seq1" ~: pfnorm ( PFNode PFNull 
                      [PFNode pfa [PFNode pfb 
                                          [PFNode pfc 
                                                  [PFNode pfd [] 4] 
                                                  4] 
                                           4,
                                   PFNode pfc 
                                          [PFNode pfd [pflb4] 4,
                                           PFNode pfb [pfld4] 4 ] 4]  
                              4,
                       PFNode pfc [
                                   PFNode pfd 
                                          [PFNode pfa [pflb4] 4] 4 ,
                                   PFNode pfa 
                                          [PFNode pfb 
                                                  [pfld4] 4 ,
                                           PFNode pfd 
                                                  [pflb4] 4 ] 4 ]
                              4
                           ]
                      8  )
                ~=? pfnorm ( pfshuffle (PFNode pfa [PFNode pfb [] 4] 4)
                                      (PFNode pfc [PFNode pfd [] 4] 4)  )
                ]


pfShuffleChoiceTests = [
    "choiceSeq1" ~: pfnorm ( PFNode PFNull [PFNode pfa [pfle4] 2,
                                   PFNode pfb [PFNode pfc [pfle4] 1,
                                               PFNode pfe [pflc] 4 ] 1,
                                   PFNode pfd [pfle4] 1,
                                   PFNode pfe [pfla2,
                                               PFNode pfb [pflc] 1,
                                               pfld ] 4 ]
                                   8 )
                ~=? pfnorm ( pfshuffle (PFNode PFNull [pfla2, 
                                             PFNode pfb [pflc] 1,
                                             pfld ] 4)
                              pfle4 ),
    "choiceChoice1" ~: pfnorm ( PFNode PFNull [PFNode pfa [pfld2,pfle2] 2,
                                      PFNode pfb [PFNode pfc [pfld2,pfle2] 2,
                                                  PFNode pfd [pflc2] 2,
                                                  PFNode pfe [pflc2] 2] 1,
                                      PFNode pfd [pfla2,
                                                  PFNode pfb [pflc2] 1 ] 2,
                                      PFNode pfe [pfla2,
                                                  PFNode pfb [pflc2] 1] 2
                                       ] 
                              6 )
               ~=? pfnorm ( pfshuffle (PFNode PFNull [pfla2, 
                                             PFNode pfb [pflc2] 1] 2)
                             (PFNode PFNull [pfld2,pfle2] 4) ) , 
     "choiceChoiceSeq1" ~: pfnorm
            (PFNode PFNull [PFNode pfa [pfld,
                                       PFNode pfe [pflf] 1] 1.0,
                           PFNode pfb [PFNode pfc [pfld,
                                                   PFNode pfe [pflf] 1] 1,
                                       PFNode pfd [pflc] 1,
                                       PFNode pfe [PFNode pff [pflc] 1,
                                                   PFNode pfc [pflf] 1] 1] 1,
                           PFNode pfd [pfla,
                                       PFNode pfb [pflc] 1] 1,
                           PFNode pfe [PFNode pff [pfla,
                                                   PFNode pfb [pflc] 1] 1,
                                       PFNode pfa [pflf] 1,
                                       PFNode pfb 
                                              [PFNode pfc [pflf] 1,
                                               PFNode pff [pflc] 1] 1] 1] 
                            4 )
               ~=? pfnorm (pfshuffle (PFNode PFNull [pfla, 
                                             PFNode pfb [pflc] 1] 2)
                             (PFNode PFNull [pfld, 
                                             PFNode pfe [pflf] 1] 2) ) ,
      "leafSilentTree" ~: 
            PFNode PFNull [PFNode pfa [PFNode PFSilent [pflb,pflc4] 5] 1,
                           PFNode PFSilent [PFNode pfa [pflb,pflc4] 1,
                                            PFNode pfb [pfla] 1,
                                            PFNode pfc [pfla] 4] 5 ] 
                   6
               ~=? pfshuffle pfla (PFNode PFSilent [pflb,pflc4] 5) 
    ] 


    



pfShuffleTests = pfSingleShuffleTests ++ pfCollapseTests 
              ++ pfShuffleSeqTests    ++ pfShuffleChoiceTests

pfTests = pfProbTests ++ pathsetPFTests ++ pfShuffleTests

--
teleclaimsEg = seqP [choiceP [Leaf "initiate payment" 431,
                              Leaf "initiate credit" 52] 483,
                     concP [Node1 PLoop 
                                    (Leaf "advise claimant" 235) 2 235, 
                            Leaf "close claim" 248] 483 ]
                    483

adLoop = Node1 PLoop (Leaf "ad" 235) 2 235

teleclaimsEgTests =  let ?epsilon = eps in [ 
    "adviseProb" ~: 1/4 ~=? prob ["ad"] adLoop,
    "creditCloseBeforeAdvise1" ~: 0.020555 ~?~ 
        prob ["initiate credit", "close claim", "advise claimant" ] 
             teleclaimsEg,
    "creditCloseBeforeAdvise2" ~: 0.010278 ~?~ 
        prob ["initiate credit", "close claim", "advise claimant",
               "advise claimant"] 
             teleclaimsEg,
    "creditCloseBeforeAdvise3" ~: 0.005139 ~?~ 
        prob ["initiate credit", "close claim", "advise claimant",
              "advise claimant", "advise claimant" ] 
             teleclaimsEg,
    "creditCloseBeforeAdvise4" ~: 0.002569 ~?~ 
        prob ["initiate credit", "close claim", "advise claimant",
              "advise claimant", "advise claimant", "advise claimant" ] 
             teleclaimsEg,
    "creditCloseBeforeAdvise5" ~: 0.001285 ~?~ 
        prob ["initiate credit", "close claim", "advise claimant",
              "advise claimant", "advise claimant", "advise claimant",
              "advise claimant" ] 
             teleclaimsEg
    ]

dupeConcEg = concP [concP [la,lb] 2, 
                    concP [la,lb] 2] 4

dupeConcEg2 = concP [concP [la,la] 2, 
                     concP [lb,lb] 2] 4

dupeConcEgTests = [
    "dupeConc"  ~: [0.18055555,0.18055555,0.1388889,
                    0.18055555,0.18055555,0.1388889] 
                        ~=? map (`tokprob` dupeConcEg) 
                                ["abab","abba","aabb","baba","baab","bbaa"],
    "dupeConc2" ~: [0.1388889,0.1388889,0.22222224,
                    0.1388889,0.1388889,0.22222224]
                        ~=? map (`tokprob` dupeConcEg2) 
                                ["abab","abba","aabb","baba","baab","bbaa"]
                ]

concSilentEg1 = concP [la, concP [lb,lc,Silent 1] 3]  4
concSilentEg2 = concP [la, concP [Leaf "b" (3/2), Leaf "c" (3/2)] 3]  4

cs1prob = map (`tokprob` concSilentEg1) 
              ["abc","acb","bac","bca","cab","cba"] 
cs2prob = map (`tokprob` concSilentEg2) 
              ["abc","acb","bac","bca","cab","cba"] 

concSilentEgTests = [
    "concSilentEg" ~: (cs1prob /= cs2prob) @? "probabilities not changed"
    ]


--

wsfaTests = [ 
    "wleaf1"    ~:  wsfaFromList 1 2 [ 1 --< ("a",1)  >-- 2 ] 
                ~=? pptToWSFA la ,
    "wsilent1"  ~:  wsfaFromList 1 2 [ 1 --<* 2 >-- 2 ] 
                ~=? pptToWSFA (Silent 2::PPTree Int),
    "wseq1"     ~:  wsfaFromList 1 4 [ 1 --< ("a",1) >-- 2,
                                       2 --< ("b",1) >-- 4] 
                ~=? pptToWSFA (NodeN Seq [la, lb] 1),
    "wchoice1"  ~:  wsfaFromList 1 4 [ 1 --< ("a",2) >-- 4,
                                       1 --< ("b",1) >-- 4] 
                ~=? pptToWSFA (NodeN Choice [la2, lb] 3),
    "wfloop1"   ~:  wsfaFromList 1 4 [ 1 --< ("a",2) >-- 2,
                                       2 --< ("a",2) >-- 3,
                                       3 --< ("a",2) >-- 4 ]
                ~=? pptToWSFA (Node1 FLoop la2 3 3)
    ]

--

concTests = probBasicConcTests
           ++ concSimpleTests
           ++ concCompoundTests
           ++ concCompoundSilentTests
           ++ concLoopTests
           ++ concSingleCollapse

utilTests = elemTests ++ permuteTests ++ loudTests

probTests = probBasicTests ++ pathsetSingletons 
            ++ probLoopTests ++ fixedLoopTests 
            ++ compoundLoopTests
            ++ loopApproxKTests
            ++ concTests 
            ++ probDuplicateTests

paperExampleTests = dupeConcEgTests ++ teleclaimsEgTests ++ concSilentEgTests


huTests = probTests ++ utilTests ++ shuffleProbTests ++ pfTests 
       ++ paperExampleTests
       ++ wsfaTests



