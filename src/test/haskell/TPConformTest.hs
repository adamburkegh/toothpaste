{-# LANGUAGE ImplicitParams #-}

module TPConformTest where

import qualified Data.Set as Set

import ProbProcessTree
import TPConform

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

(~~~) :: Float -> Float -> Bool
(~~~) x y = (x-y) < eps

acmp :: (Eq a) => PPTree a -> PPTree a -> Bool
acmp (Leaf a n) (Leaf b m) = a == b && n ~~~ m
acmp (Silent n) (Silent m) = n ~~~ m
acmp (Node1 p1 a r1 n) (Node1 p2 b r2 m)
        = (p1 == p2) && (acmp a b) && (r1 ~~~ r2) && (n ~~~ m)
acmp (NodeN po1 ptl1 n) (NodeN po2 ptl2 m)
        = (po1 == po2) && (acmpl ptl1 ptl2) && (n ~~~ m)
acmp x y = False

acmpl :: (Eq a) => [PPTree a] -> [PPTree a] -> Bool
acmpl ptl1 ptl2 =  length ptl1  == length ptl2
        && foldl (\c (pt1,pt2) -> (acmp pt1 pt2) && c) True z
    where z = zip ptl1 ptl2


tokprob :: String -> PPTree String -> Float
tokprob s pt = prob (map (\x -> [x]) s) pt


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
            "compSeq1" ~: 1/6 ~=? prob ["a","b","c","d"] cSeq2,
            "compSeq2" ~: 1/2 ~=? prob ["d","a","b","c"] cSeq2,
            "compSeqInter1" ~: 1/6 ~=? prob ["a","d","b","c"] cSeq2,
            "compSeqInter2" ~: 1/6 ~=? prob ["a","b","d","c"] cSeq2,
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
            "compChoice2-3" ~: 7/30 ~?~ prob ["c","d" ] cChoice2,
            "compChoice2-4" ~: 7/30 ~?~ prob ["d","c" ] cChoice2,
            "compChoice2-5" ~: 1/10 ~?~ prob ["c","a","d"] cChoice2,
            "compChoice2-6" ~: 1/30 ~?~ prob ["c","d","a"] cChoice2,
            "compChoice2-7" ~: 1/10 ~?~ prob ["d","a","c"] cChoice2,
            "compChoice2-8" ~: 1/30 ~?~ prob ["d","c","a"] cChoice2,
            "compChoice2-9" ~: 0.0  ~=? prob ["a","b"] cChoice2 ]

cConc1 = NodeN Conc [la, NodeN Conc [lb4,lc] 5] 6
concConcCompound = let ?epsilon = eps in [
        "concc1" ~: 0 ~?~ prob ["a","b","c"] cConc1 -- TODO wrong
    ]

concCompoundTests = concSeqCompound
                    -- TODO ++ concChoiceCompound1 ++ concChoiceCompound2

cSil = NodeN Conc [ NodeN Seq [la,Silent 1,lb] 1,
                    lc ] 2
concCompoundSilentTests = [
            "compSeq1" ~: 1/4 ~=? prob ["a","b","c"] cSil,
            "compSeq2" ~: 1/2 ~=? prob ["c","a","b"] cSil,
            "compSeqInter" ~: 1/4 ~=? prob ["a","c","b"] cSil,
            "compSeqInvalidOrder1" ~: 0 ~=? prob ["b","a","c"] cSil ,
            "compSeqInvalidOrder2" ~: 0 ~=? prob ["b","c","a"] cSil ,
            "compSeqInvalidOrder3" ~: 0 ~=? prob ["c","b","a"] cSil ,
            "compSeqInvalidDupe" ~: 0 ~=? prob ["a","b","c","c"] cSil ]


probDuplicateTests = [ "incomplete" ~: 0 ~=? 1 ]

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

loudTests = [ "silent" ~: False ~=? loud (Silent 1),
              "leaf" ~:   True  ~=? loud la,
              "choice" ~: True  ~=? loud (NodeN Choice [la,lb] 2),
              "ploop" ~:  False ~=? loud (Node1 PLoop la 3 1) ]


isPathsetTests = [ "silent" ~: True ~=? isPathset (Silent 1),
                   "leaf" ~:   True  ~=? isPathset la,
                   "choice" ~: True  ~=? isPathset (NodeN Choice [la,lb] 2),
                   "conc" ~: False  ~=? isPathset (NodeN Conc [la,lb] 2),
                   "seq" ~: True  ~=? isPathset (NodeN Seq [la,lb] 2),
                   "ploop" ~:  False ~=? isPathset (Node1 PLoop la 3 1) ]

pathsetBasicTests = [ 
            "silent" ~: (Silent 3) ~=? pathset ((Silent 3)::(PPTree String)),
            "leaf"   ~: la ~=? pathset la,
            "seq"    ~: NodeN Seq [la,lb] 1 ~=? pathset (NodeN Seq [la,lb] 1),
            "choice" ~: NodeN Choice [la,lb] 2 
                            ~=? pathset (NodeN Choice [la,lb] 2), 
            "floop" ~: NodeN Seq [la,la,la] 1 
                            ~=? pathset (Node1 FLoop la 3 1)  ]
               
pathsetPLoopTests = [ "leaf" ~: 
         acmp (NodeN Seq [Silent 10,
                    NodeN Choice 
                          [scale (NodeN Seq [la10,la10,la10,la10,Silent 10] 10) 
                                 ((2**4)/(3**5)),
                           scale (NodeN Seq [la10,la10,la10,Silent 10] 10) 
                                 ((2**3)/(3**4)),
                           scale (NodeN Seq [la10,la10,Silent 10] 10) 
                                 ((2*2)/(3**3)),
                           scale (NodeN Seq [la10,Silent 10] 10) (2/(3*3)),
                           Silent (10/3)] 
                           10] 10)
              (pathsetEps (Node1 PLoop la10 3 10) 0.4) 
              @? "pathset mismatch" ]

pathsetConcTests = [ 
    "twoLeaves" ~: NodeN Seq 
                         [Silent 2, 
                          NodeN Choice 
                                [NodeN Seq [la,lb] 1, 
                                 NodeN Seq [lb,la] 1] 2] 2 
               ~=? pathsetConc (NodeN Conc [la,lb] 2) eps,
    "oneLeafOneSilent" ~: NodeN Seq 
                                [Silent 2, 
                                 NodeN Choice 
                                       [NodeN Seq [la,Silent 1] 1, 
                                        NodeN Seq [Silent 1,la] 1] 2] 2 
               ~=? pathsetConc (NodeN Conc [la,Silent 1] 2) eps,
    "twoLeafOneSilent" ~: 
        NodeN Seq 
              [Silent 6, 
               NodeN Choice 
                     [NodeN Seq [la2,
                                 choiceP [seqP [lb,Silent 1] 1,
                                          seqP [Silent 1,lb] 1 ] 2] 2,
                      NodeN Seq [lb2,
                                 choiceP [seqP [la,Silent 1] 1,
                                          seqP [Silent 1,la] 1] 2] 2,
                      NodeN Seq [Silent 2,
                                 choiceP [seqP [la,lb] 1,
                                          seqP [lb,la] 1] 2] 2
                                 ] 6] 
               6
               ~=? pathsetConc (NodeN Conc [la2,Silent 2, lb2] 6) eps
                    ]

--

cab = choiceP [la,lb] 2
ccd = choiceP [lc,ld] 2

shuffleSingleTests = [
    "leaves" ~: choiceP [NodeN Seq [la,lb] 1,NodeN Seq [lb,la] 1] 2
                ~=? shuffle la lb,
    "leavesUneven" ~: choiceP [NodeN Seq [la,lb] 1,NodeN Seq [lb2,la2] 2] 3
                ~=? shuffle la lb2 ,
    "leafSeq" ~: choiceP [NodeN Seq [la2,lb2,lc2] 2,
                          NodeN Seq [lb,
                                     choiceP [scale (seqP [la,lc] 1) (2/3),
                                              scale (seqP [lc,la] 1) (1/3)] 1] 
                                1] 3
                ~=? shuffle la2 (NodeN Seq [lb,lc] 1),
    "silentSeq" ~: choiceP [NodeN Seq [Silent 2,lb2,lc2] 2,
                            NodeN Seq [lb,
                                     choiceP [scale (seqP [Silent 1,lc] 1) 
                                                    (2/3),
                                              scale (seqP [lc,Silent 1] 1) 
                                                    (1/3)] 1] 
                                1] 3
                ~=? shuffle (Silent 2) (NodeN Seq [lb,lc] 1),
    "choiceLeaf" ~: choiceP[seqP [la2,lc2] 2,
                            seqP [lb2,lc2] 2,
                            seqP [lc4, choiceP [la2,lb2] 4] 4] 8
                ~=? shuffle (choiceP [la2,lb2] 4) lc4,
    "twoTermChoices" ~: choiceP[ seqP [la,scale (choiceP [lc,ld] 2) 0.5] 1, 
                                 seqP [lb,scale (choiceP [lc,ld] 2) 0.5] 1, 
                                 seqP [lc,scale (choiceP [la,lb] 2) 0.5] 1, 
                                 seqP [ld,scale (choiceP [la,lb] 2) 0.5] 1] 4 
                ~=? shuffle cab ccd
                ]

shuffleSeqTests = [
    "seq1" ~: choiceP [seqP [la4,
                             choiceP [seqP [lb2,lc2,ld2] 2,
                                      seqP [lc2,
                                            choiceP[seqP [lb,ld] 1,
                                                    seqP [ld,lb] 1] 2] 2] 4] 4,
                       seqP [lc4,
                             choiceP [seqP [ld2,la2,lb2] 2,
                                      seqP [la2,
                                            choiceP[seqP [lb,ld] 1,
                                                    seqP [ld,lb] 1] 2] 2] 4] 4 
                       ] 8
                ~=? shuffle (NodeN Seq [la4,lb4] 4) 
                            (NodeN Seq [lc4,ld4] 4)
                ]

shuffleChoiceTests = [
    "choiceSeq1" ~: choiceP [seqP [la2,le2] 2, 
                             seqP [lb,
                                   scale (choiceP [seqP [lc2,le2] 2,
                                                   seqP [le8,lc8] 8] 10)
                                         (1/10)]
                                   1,
                             seqP [ld,le] 1, 
                             seqP [le4,choiceP[la2,ld,
                                               seqP [lb,lc] 1] 4] 4 ]
                            8
                ~=? shuffle (choiceP [la2, seqP [lb,lc] 1,ld] 4)
                            le4 ,
    "choiceChoice1" ~: 
        choiceP [seqP [la2,choiceP [ld,le] 2] 2,
                 seqP [lb2,
                       scale (choiceP [ seqP [lc2,
                                              choiceP [ld,le] 2] 2,
                                        seqP [ld2,lc2] 2,
                                        seqP [le2,lc2] 2] 6 )
                             (1/3)] 2,
                 seqP [ld2,choiceP [la,
                                    seqP [lb,lc] 1] 2 ] 2,
                 seqP [le2,choiceP [la,
                                    seqP [lb,lc] 1] 2 ] 2
                      ] 8
                ~=? shuffle (choiceP [la2, seqP [lb2,lc2] 2] 4)
                            (choiceP [ld2, le2] 4) ,
    "choiceChoiceSeq1" ~: 
                    choiceP [seqP [la,scale (choiceP [ld,seqP [le,lf] 1 ] 2)
                                             (1/2)] 1, 
                             seqP [lb, 
                                   scale (
                                        choiceP [ seqP [lc2, 
                                                      choiceP [ld,
                                                               seqP [le,
                                                                     lf] 1] 2 
                                                        ] 2,
                                             seqP [ld2,lc2] 2,
                                             seqP [le2,
                                                   choiceP [ seqP [lc,lf] 1,
                                                             seqP [lf,lc] 1] 
                                                             2
                                                   ] 2] 6 ) 
                                        (1/6)] 1,
                             seqP [ld, scale (choiceP [la, 
                                                       seqP [lb,lc] 1 ] 2)
                                             (1/2) ] 1, 
                             seqP [le, 
                                   scale (
                                        choiceP [ 
                                             seqP [la2,lf2] 2,
                                             seqP [lb2,
                                                   choiceP [ seqP [lc,lf] 1,
                                                             seqP [lf,lc] 1] 
                                                             2 
                                                  ] 2,
                                             seqP [lf2,
                                                      choiceP [la,
                                                               seqP [lb,
                                                                     lc] 1] 2 
                                                        ] 2
                                                   ] 6 ) 
                                        (1/6)] 1 ] 
                            4
                ~=? shuffle (choiceP [la, seqP [lb,lc] 1] 2)
                            (choiceP [ld, seqP [le,lf] 1] 2) 
    ] 

cc1 =  shuffle (choiceP [la2, seqP [lb2,lc2] 2] 4)
                            (choiceP [ld2, le2] 4)
shuffleProbChoiceChoiceTests = [ 
    "choiceChoice1p1"  ~: 1/8 ~=? tokprob "ad" cc1 ,
    "choiceChoice1p2"  ~: 1/8 ~=? tokprob "ae" cc1 ,
    "choiceChoice1p3"  ~: 1/24 ~=? tokprob "bce" cc1 ,
    "choiceChoice1p4"  ~: 1/24 ~=? tokprob "bcd" cc1 ,
    "choiceChoice1p5"  ~: 1/12 ~=? tokprob "bdc" cc1 ,
    "choiceChoice1p6"  ~: 1/12 ~=? tokprob "bec" cc1 ,
    "choiceChoice1p7"  ~: 1/8 ~=? tokprob "da" cc1 ,
    "choiceChoice1p8"  ~: 1/8 ~=? tokprob "dbc" cc1 ,
    "choiceChoice1p9"  ~: 1/8 ~=? tokprob "ea" cc1,
    "choiceChoice1p10" ~: 1/8 ~=? tokprob "ebc" cc1
    ]
    
cl1 = shuffle (choiceP [la2,lb2] 4) lc4
shuffleProbChoiceLeafTests1 = [
    "cl1" ~: 0 ~=? tokprob "ab" cl1,
    "cl2" ~: 1/4 ~=? tokprob "ac" cl1,
    "cl3" ~: 1/4 ~=? tokprob "bc" cl1,
    "cl4" ~: 1/4 ~=? tokprob "ca" cl1,
    "cl5" ~: 1/4 ~=? tokprob "cb" cl1
    ]

    
cl2 = shuffle (choiceP [la2,lb2] 4) lc2
shuffleProbChoiceLeafTests2 = [
    "cl1" ~: 0 ~=? tokprob "ab" cl2,
    "cl2" ~: 1/3 ~=? tokprob "ac" cl2,
    "cl3" ~: 1/3 ~=? tokprob "bc" cl2,
    "cl4" ~: 1/6 ~=? tokprob "ca" cl2,
    "cl5" ~: 1/6 ~=? tokprob "cb" cl2
    ]

shuffleProbChoiceTermTests = [
        "ct1" ~:  all (\p -> p == 1/8)
                (map (\s -> tokprob s (shuffle cab ccd) ) 
                    ["ac","ad","bc","bd","ca","cb","da","db"] ) 
                    @? "1/8 expected"
    ]

cs1 = shuffle (choiceP [la2, seqP [lb,lc] 1,ld] 4) le4 
shuffleProbChoiceSeqTests = [
    "cs1" ~: 1/4 ~=? tokprob "ae" cs1,
    "cs2" ~: 1/40 ~=? tokprob "bce" cs1,
    "cs3" ~: 1/10 ~=? tokprob "bec" cs1,
    "cs4" ~: 1/8 ~=? tokprob "de" cs1,
    "cs5" ~: 1/4 ~=? tokprob "ea" cs1,
    "cs6" ~: 1/8 ~=? tokprob "ebc" cs1,
    "cs7" ~: 1/8 ~=? tokprob "ed" cs1
    ] 

ccs1 = shuffle (choiceP [la, seqP [lb,lc] 1] 2)
               (choiceP [ld, seqP [le,lf] 1] 2) 
shuffleProbChoiceChoiceSeqTests = [
    "cs1" ~:  1/8 ~=?  tokprob "ad"   ccs1,
    "cs2" ~:  1/8 ~=?  tokprob "aef"  ccs1,
    "cs3" ~:  1/12 ~=? tokprob "bdc"  ccs1,
    "cs4" ~:  1/24 ~=? tokprob "becf" ccs1,
    "cs5" ~:  1/24 ~=? tokprob "bcef" ccs1,
    "cs6" ~:  1/24 ~=? tokprob "bcd"  ccs1,
    "cs7" ~:  1/8 ~=?  tokprob "da"   ccs1,
    "cs8" ~:  1/8 ~=?  tokprob "dbc"  ccs1,
    "cs9" ~:  1/12 ~=? tokprob "eaf"  ccs1,
    "cs10" ~: 1/24 ~=? tokprob "efa"  ccs1,
    "cs11" ~: 1/24 ~=? tokprob "efbc"  ccs1,
    "cs12" ~: 1/24 ~=? tokprob "ebfc"  ccs1,
    "cs13" ~: 1/24 ~=? tokprob "ebcf"  ccs1
    ]

shuffleProbTests = shuffleProbChoiceLeafTests1 ++ shuffleProbChoiceLeafTests2 
    ++ shuffleProbChoiceChoiceTests 
    ++ shuffleProbChoiceTermTests
    ++ shuffleProbChoiceSeqTests
    ++ shuffleProbChoiceChoiceSeqTests

shuffleTests = shuffleSingleTests ++ shuffleSeqTests ++ shuffleChoiceTests
    ++ shuffleProbTests

--
pfa = PFSymbol 'a'
pfb = PFSymbol 'b'



pfProbTests = [ 
    "leaf1"     ~: 1   ~=? pfprob "a" (PFNode (pfa) [] 1),
    "leaf2"     ~: 0   ~=? pfprob "b" (PFNode (pfa) [] 1),
    "silent1"   ~: 1   ~=? pfprob ""  (PFNode (PFSilent) [] 1),
    "silent2"   ~: 0   ~=? pfprob "b" (PFNode (PFSilent) [] 1),
    "choice1"   ~: 1/2 ~=? pfprob "a" 
                                  (PFNode (pfa) 
                                          [PFNode PFSilent [] 1,
                                           PFNode (pfb) [] 1] 5),
    "choice2"   ~: 1/3 ~=? pfprob "a" 
                                  (PFNode (PFSilent) 
                                          [PFNode (pfa) [] 1,
                                           PFNode (pfb) [] 2] 5),
    "choice2"   ~: 2/3 ~=? pfprob "ab" 
                                  (PFNode (pfa) 
                                          [PFNode (PFSilent) [] 1,
                                           PFNode (pfb) [] 2] 5) 
    ]

pfTests = pfProbTests 


--

concTests = probBasicConcTests
           ++ concSimpleTests
           -- ++ concCompoundTests
           -- ++ concCompoundSilentTests


utilTests = elemTests ++ permuteTests ++ loudTests

pathsetTests = isPathsetTests ++ pathsetBasicTests ++ pathsetPLoopTests
            ++ pathsetConcTests

probTests = probBasicTests ++ probLoopTests ++ fixedLoopTests 
            ++ loopApproxKTests
            ++ pathsetTests
            ++ concTests 

huTests = probTests ++ utilTests ++ shuffleTests ++ pfTests



