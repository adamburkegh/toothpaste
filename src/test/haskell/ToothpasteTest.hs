module ToothpasteTest where

import ProbProcessTree
import Toothpaste hiding (main)

import PPTTestUtil

import Data.Set (Set,toList,fromList)
import Test.HUnit



la  = Leaf "a" 1
lb  = Leaf "b" 1
lc  = Leaf "c" 1
ld  = Leaf "d" 1
ld4 = Leaf "d" 4
le  = Leaf "e" 1
lf  = Leaf "f" 1
lg  = Leaf "g" 1
cab = NodeN Choice [la,lb] 2
cab3 = NodeN Choice [NodeN Seq [lb,la] 1,NodeN Seq [lb,lb] 1] 2
cab4 = NodeN Choice [NodeN Seq [lb,la,lc] 1,NodeN Seq [lb,lb,lb] 1] 2
cabc1 = NodeN Choice [la,lb,lc] 1
cba2 = NodeN Choice [NodeN Seq [lb,la] 1,NodeN Seq [lb,la] 1] 2

saa = NodeN Seq [la,la] 1
saaa = NodeN Seq [la,la,la] 1
saaat = NodeN Seq [la,NodeN Seq [la,la] 1] 1
sab  = NodeN Seq [la,lb] 1
sabc  = NodeN Seq [la,lb,lc] 1
sabcd  = NodeN Seq [la,lb,lc,ld] 1
sade = NodeN Seq [la,ld,le] 1
sablc = seqP [la, lb, ploop lc 4 1] 1
sba  = NodeN Seq [lb,la] 1
sba2 = NodeN Seq [lb2,la2] 2
sbc2 = seqP [lb2,lc2] 2
sbad2  = NodeN Seq [lb2,la2,ld2] 2
sca = seqP [lc,la] 1
scab = seqP [lc,la,lb] 1
scb2 = seqP [lc2,lb2] 2
sdba2 = seqP [ld2,lb2,la2] 2
sdlba2 = seqP [ld2,ploop lb2 4 2,la2] 2
sdb2  = seqP [ld2,lb2] 2
sefgd = seqP [le,lf,lg,ld] 1

cbacla = choiceP[seqP [lb,la] 1,
                 seqP [lc,Node1 PLoop la 3 1] 1 ] 2
ccab1 = NodeN Conc [la,lb] 2

la2 = Leaf "a" 2
la3 = Leaf "a" 3
la4 = Leaf "a" 4
la5 = Leaf "a" 5
la11 = Leaf "a" 11
lb2 = Leaf "b" 2
lb3 = Leaf "b" 3
lb4 = Leaf "b" 4
lc2 = Leaf "c" 2
lc3 = Leaf "c" 3
ld2 = Leaf "d" 2
looppa1 = Node1 PLoop la 2 1
looppa2 = Node1 PLoop la2 2 2
l4pa1   = Node1 PLoop la 4 1
sabcld = NodeN Seq [la,lb,lc, Node1 PLoop ld 3 1] 1
slab   = NodeN Seq [l4pa1,lb] 1
slabc  = NodeN Seq [l4pa1,lb,lc] 1
slbla2  = NodeN Seq [ploop lb2 3 2,ploop la2 6 2] 2


silentSingleRuleList :: (Eq a, Ord a) => [TRule a]
silentSingleRuleList = [
            TRule{rulename="silentSeq",trule=silentSeq},
            TRule{rulename="singleNodeOp",trule=singleNodeOp}
            ]

ssRuleOrdered x = maxTransformRuleOrder x silentSingleRuleList

-- Tests

eqTests = ["eqLeaf1" ~: Leaf "a" 1 ~=? Leaf "a" 1,
           "eqLeaf2" ~: Leaf "a" 2 /= Leaf "b" 2 @? "neq",
           "eqLeaf3" ~: Leaf "a" 2 /= cab @? "neq",
           "eqLeaf4"     ~: Leaf "a" 1 /= Leaf "a" 2 @? "neq",
           "neqSilent1"  ~: silent 1 /= silent 2 @? "neq",
           "eqSilent"    ~: silent 0 ~=? emptyTree,
           "eqNode1" ~: cab ~=? cab,
           "eqNode2" ~: cab /= cba2 @? "neq"]


-- Meta rule tests
adjMergeTests = [ 
        "empty"     ~: ([]::[Int])  ~=? adjMerge (==) (+) ([]::[Int]),
        "single"    ~: [1]       ~=? adjMerge (==) (+) [1],
        "double"    ~: [2]       ~=? adjMerge (==) (+) [1,1],
        "double2"   ~: [6]       ~=? adjMerge (==) (+) [3,3],
        "doubleNo"  ~: [1,2]       ~=? adjMerge (==) (+) [1,2],
        "tripleNo"  ~: [1,2,3]       ~=? adjMerge (==) (+) [1,2,3],
        "tripleSame"  ~: [6,3]       ~=? adjMerge (==) (+) [3,3,3],
        "tripleAll"   ~: [9]       ~=? adjMerge (>=) (+) [3,3,3],
        "tripleHead"  ~: [10,2]       ~=? adjMerge (==) (+) [5,5,2],
        "tripleMid"   ~: [1,10]       ~=? adjMerge (==) (+) [1,5,5],
        "tripleSplit" ~: [1,5,1]    ~=? adjMerge (==) (+) [1,5,1],
        "fullReduce"  ~: ["abc"]    
            ~=? adjMerge (\x y -> True) (++) ["a","b","c"],
        "mergeCausesDissim"  ~: ["aa","aa"]    
            ~=? adjMerge (==) (++) ["a","a","a","a"]
    ]


-- Meta rule tests
anyMergeTests = [ 
        "empty"     ~: ([]::[Int])  ~=? anyMerge (==) (+) ([]::[Int]),
        "single"    ~: [1]       ~=? anyMerge (==) (+) [1],
        "double"    ~: [2]       ~=? anyMerge (==) (+) [1,1],
        "double2"   ~: [6]       ~=? anyMerge (==) (+) [3,3],
        "doubleNo"  ~: [1,2]       ~=? anyMerge (==) (+) [1,2],
        "tripleNo"  ~: [1,2,3]       ~=? anyMerge (==) (+) [1,2,3],
        "tripleSame"  ~: [6,3]       ~=? anyMerge (==) (+) [3,3,3],
        "tripleAll"   ~: [6,3]       ~=? anyMerge (<=) (+) [3,3,3],
        "tripleHead"  ~: [10,2]       ~=? anyMerge (==) (+) [5,5,2],
        "tripleMid"   ~: [1,10]       ~=? anyMerge (==) (+) [1,5,5],
        "tripleSplit" ~: [5,2]    ~=? anyMerge (==) (+) [1,5,1],
        "quadSplit1"  ~: [5,2,5]   ~=? anyMerge (==) (+) [1,5,1,5], 
        "quad2"       ~: [5,5,2]   ~=? anyMerge (==) (+) [1,5,5,1],
        "fullReduce"  ~: ["ab","c"]    
            ~=? anyMerge (\x y -> True) (++) ["a","b","c"],
        "mergeCausesDissim"  ~: ["aa", "a","a","a"]    
            ~=? anyMerge (==) (++) ["a","a","a","a","a"],
        "mergeCausesDissim2"  ~: ["aa","a","b"]    
            ~=? anyMerge (==) (++) ["a","a","a","b"],
        "mergeCausesDissim3"  ~: ["aza","za","b"]    
            ~=? anyMerge (\x y -> x == "a" && y == "za") 
                         (++) 
                         ["a","za","za","b"]
    ]


-- Rule tests

silentSeqTests = [
    "silentSeqNoop" ~: ccab1 ~=? silentSeq ccab1,
    "silentSeq2Seq" ~: NodeN Seq [la] 1
                        ~=? silentSeq (NodeN Seq [la,Silent 1] 1),
    "silentSeq3Seq" ~: NodeN Seq [la,lb] 1 
                        ~=? silentSeq (NodeN Seq [la,Silent 1,lb] 1)  ]

silentConcTests = [
    "silentConcNoop" ~: ccab1 ~=? silentConc ccab1,
    "silentConc2Seq" ~: NodeN Conc [la] 1
                        ~=? silentConc (NodeN Conc [la,Silent 1] 1),
    "silentConc3Seq" ~: NodeN Conc [la,lb] 1 
                        ~=? silentConc (NodeN Conc [la,Silent 1,lb] 1)  ]

singleNodeOpTests = [ 
    "singleNodeNoop"  ~: cba2 ~=? singleNodeOp cba2 ,
    "singleNodeOpSeq" ~: la ~=? singleNodeOp (NodeN Seq [la] 1)  ]

choiceSimTests = ["choiceSim1" ~: la2 
                        ~=? choiceSim(  NodeN Choice [la,la] 2),
                 "choiceSim2" ~: cab ~=? choiceSim cab ,
                 "choiceSim3" ~: NodeN Seq [lb2,la2] 2
                        ~=? choiceSim cba2,
                 "choiceSim4" ~: cab3 ~=? choiceSim cab3 ,
                 "choiceSimLoop" ~: Node1 PLoop la2 6 2
                    ~=? choiceSim (NodeN Choice [Node1 PLoop la 7 1,
                                                 Node1 PLoop la 5 1] 2) ]

loopSimTests = [ 
    "loooSimLeaf" ~: la2 ~=? loopSim la2,
    "loopSim2Loops" ~: NodeN Choice [looppa1,looppa1] 2
                        ~=? loopSim (NodeN Choice [looppa1,looppa1] 2),
    "loopSimLoopAndLeaf" ~: Node1 PLoop la2 1.5 2
                        ~=? loopSim (NodeN Choice [looppa1,la] 2),
    "loopSimLoopAndLeaf2" ~: Node1 PLoop la2 1.5 2
                        ~=? loopSim (NodeN Choice [la,looppa1] 2),
    "loopSimLoopAndLeaf3" ~: choiceP [Node1 PLoop la2 1.5 2,lb] 3
                        ~=? loopSim (NodeN Choice [la,looppa1,lb] 3),
    "loopSim2" ~: cab ~=? loopSim cab ,
    "loopSim3" ~: Node1 PLoop (NodeN Seq [lb2,la2] 2) 2 2
        ~=? loopSim (choiceP [seqP [lb,la] 1,
                              Node1 PLoop (seqP [lb,la] 1) 3 1] 
                              2),
                 "loopSim4" ~: cab3 ~=? choiceSim cab3 ,
    "choiceRoll2" ~: Node1 PLoop la4 4 4 ~=?
                loopSim (NodeN Choice [la,
                                       Node1 PLoop la3 5 3] 4),
    "choiceRoll3" ~: Node1 PLoop la3 1.3333334 3.0 ~=?
                loopSim (NodeN Choice [Node1 PLoop la 2.0 1.0,
                                       la2]
                               3.0), 
    "choiceRoll4" ~: 
        choiceP [Node1 PLoop la3 1.3333334 3.0,
                 lb] 4.0 
            ~=? loopSim (NodeN Choice [Node1 PLoop la 2.0 1.0,
                                          la2,
                                          lb]
                                         4.0) ,
    "loopSimMergeDissim" ~: 
        NodeN Choice [ploop la 4 1,
                      ploop la2 2 2, 
                      seqP [ploop la 3 1, lb, la] 1] 4 
            ~=? loopSim (NodeN Choice [la, 
                                       ploop la 3 1, 
                                       ploop la 4 1, 
                                       seqP [ploop la 3 1, lb, la] 1] 4 )
               ]

concSimTests = [
    "concSim1" ~: la ~=? concSim la,
    "concSim2" ~: NodeN Conc [Node1 FLoop la2 2 2] 2 
                        ~=? concSim(NodeN Conc [la,la] 2) ,
    "concSim3" ~: NodeN Conc [la,lb] 2 ~=? concSim(NodeN Conc [la,lb] 2)  
                ]


loopConcSimTests = [
    "lconcSim1" ~: la ~=? loopConcSim la,
    "lconcSim2" ~: NodeN Conc [Node1 FLoop (Node1 PLoop la2 1.5 2) 2 2] 2 
                        ~=? loopConcSim(NodeN Conc [la,looppa1] 2) ,
    "lconcSim3" ~: NodeN Conc [la,lb] 2 ~=? loopConcSim(NodeN Conc [la,lb] 2),  
    "lconcSim4" ~: NodeN Conc [Node1 FLoop (Node1 PLoop la2 1.5 2) 2 2] 2 
                        ~=? loopConcSim(NodeN Conc [la,looppa1] 2) 
                ]

choiceFoldPrefixTests = [
    "choiceFold1" ~: choiceFoldPrefix la ~=? la,
    "choiceFold2" ~: choiceFoldPrefix cab  ~=? cab,
    "choiceFoldEmptyTailSeq" ~: 
        NodeN Seq [lb2, NodeN Choice [la,la] 2] 2
                            ~=? choiceFoldPrefix cba2 ,
    "choiceFoldLeftoverSeq" ~: 
            NodeN Seq [lb2,
                      NodeN Choice [NodeN Seq [la,lc] 1,
                                    NodeN Seq [lb,lb] 1] 2] 
              2
                            ~=? choiceFoldPrefix cab4 ,
    "choiceFoldPrefixMore4Choices" ~:
             choiceP [NodeN Seq [la,lb] 1,
                      NodeN Seq [lb2, 
                                 NodeN Choice [lc,ld] 2] 2,
                      NodeN Seq [la,lb] 1 ] 4
            ~=? choiceFoldPrefix (NodeN Choice [NodeN Seq [la,lb] 1,
                                                NodeN Seq [lb,lc] 1,
                                                NodeN Seq [lb,ld] 1,
                                                NodeN Seq [la,lb] 1] 4),
    "choiceFoldId1" ~: choiceSim (choiceFoldPrefix cba2) /= 
                            choiceFoldPrefix (choiceSim cba2) @? "neq" ]


choiceFoldSuffixTests = [
    "choiceFold1" ~: choiceFoldSuffix la ~=? la,
    "choiceFold2" ~: choiceFoldSuffix cab  ~=? cab,
    "choiceFoldEmptyHeadSeq" ~: 
        NodeN Seq [NodeN Choice [lb,lb] 2, la2] 2
                            ~=? choiceFoldSuffix cba2 ,
    "choiceFoldLeftoverSeq" ~: 
            NodeN Seq [NodeN Choice [NodeN Seq [la,lb] 1,
                                     NodeN Seq [lb,la] 1] 2,
                       lc2] 2
            ~=? choiceFoldSuffix (choiceP [seqP [la,lb,lc] 1,
                                           seqP [lb,la,lc] 1] 2),
    "choiceFoldSuffixMore4Choices" ~:
             choiceP [NodeN Seq [lb,lc] 1,
                      NodeN Seq [la,lc] 1,
                      NodeN Seq [choiceP [la,la] 2,
                                 lb2] 2 ] 4
            ~=? choiceFoldSuffix (NodeN Choice [NodeN Seq [la,lb] 1,
                                                NodeN Seq [lb,lc] 1,
                                                NodeN Seq [la,lc] 1,
                                                NodeN Seq [la,lb] 1] 4),
    "choiceFoldId1" ~: choiceSim (choiceFoldSuffix cba2) /= 
                            choiceFoldPrefix (choiceSim cba2) @? "neq",
    "choiceFoldLongSuffix" ~:
           NodeN Seq [NodeN Choice [seqP [la,lb,lc] 1,
                                    seqP [le,lf,lg] 1 ] 2,
                      ld2] 2
            ~=? choiceFoldSuffix (NodeN Choice [sabcd,sefgd] 2) 
                            ]


clbca = NodeN Choice [NodeN Seq [la,lc] 1, 
                      NodeN Seq [Node1 PLoop la 1 1,
                                 lb] 1]  2

loopChoiceFoldPrefixTests = [
    "loopChoiceFold1" ~: loopChoiceFoldPrefix la ~=? la ,
    "loopChoiceFold2" ~: loopChoiceFoldPrefix cab  ~=? cab,
    "loopChoiceFold3" ~: NodeN Seq [Node1 PLoop  la2 1 2,
                                    NodeN Choice [lb,lc] 2] 
                                   2
            ~=? loopChoiceFoldPrefix clbca ,
    "loopChoiceFoldId1" ~: 
           loopSim (choiceFoldPrefix cba2) 
           /= loopChoiceFoldPrefix (choiceSim cba2) @? "neq",

    "loopChoiceFoldLongPrefix" ~:  
           NodeN Choice [ sade,
                          seqP[ ploop la2 2.5 2,
                                choiceP [NodeN Seq [lb,lc] 1, 
                                         lb] 2] 2 ] 3
            ~=? loopChoiceFoldPrefix (NodeN Choice [sabc,sade,slab] 3)
            ]

loopChoiceFoldSuffixTests = [
    "loopChoiceFold1" ~: loopChoiceFoldSuffix la ~=? la ,
    "loopChoiceFold2" ~: loopChoiceFoldSuffix cab  ~=? cab,
    "loopChoiceFold3" ~: loopChoiceFoldSuffix cbacla 
            ~=? NodeN Seq [NodeN Choice [lb,lc] 2,
                           Node1 PLoop  la2 2 2] 
                      2,
    "loopChoiceFoldId1" ~: 
           loopSim (choiceFoldPrefix cba2) 
           /= loopChoiceFoldSuffix (choiceSim cba2) @? "neq",
    "loopChoiceFoldLongSuffix" ~:  
           NodeN Seq [NodeN Choice [seqP [la,lb,lc] 1, 
                                    seqP [le,lf,lg] 1] 2,
                      Node1 PLoop ld2 2 2] 2
            ~=? loopChoiceFoldSuffix (NodeN Choice [sabcld,sefgd] 2)
                            ]  



loopChoiceFoldTests = loopChoiceFoldPrefixTests ++ loopChoiceFoldSuffixTests


choiceSkipPrefixTests = [
    "choiceSkipNone" ~: la ~=? choiceSkipPrefix la,
    "choiceSkipLeafFirst" ~: 
        NodeN Seq [la2,
                   NodeN Choice [lb,Silent 1] 2] 
              2 
                    ~=? choiceSkipPrefix (choiceP [la,seqP [la,lb] 1] 2),
    "choiceSkipSeqFirst"  ~: 
        NodeN Seq [la2,
                   NodeN Choice [lb,Silent 1] 2] 
              2 
                    ~=? choiceSkipPrefix (NodeN Choice [seqP [la,lb] 1,la] 2),
    "choiceSkip3"    ~: NodeN Choice [ NodeN Seq [la,lc] 1,
                                      NodeN Seq [la2,
                                                 NodeN Choice [lb,Silent 1] 
                                                               2] 2]
                              3 
                    ~=? choiceSkipPrefix (choiceP [la,
                                                   seqP [la,lb] 1,
                                                   seqP [la,lc] 1] 
                                                   3),
    "choiceSkipWithTail" ~: seqP [la4,
                                 choiceP [Silent 2,
                                          seqP[lb2,la2] 2 ] 4] 4
        ~=? choiceSkipPrefix (NodeN Choice [la2,
                                            seqP [la2,lb2,la2] 2.0] 4.0)
                    ]

choiceSkipPrefixCompressTests = [
    "choiceSkipNone" ~: la ~=? choiceSkipPrefixCompress la,
    "choiceSkip1"    ~: NodeN Seq [la2,
                                   NodeN Choice [lb,Silent 1] 2] 
                              2 
                    ~=? choiceSkipPrefixCompress 
                            (choiceP [la,seqP [la,lb] 1] 2),
    "choiceSkip2"    ~: NodeN Seq [la3,
                                   NodeN Choice [lb,lc,Silent 1] 3
                                   ] 
                              3 
                    ~=? choiceSkipPrefixCompress (choiceP [la,
                                                   seqP [la,lb] 1,
                                                   seqP [la,lc] 1] 
                                                   3)
                    ]


loopChoiceSkipTests = [
    "lchoiceSkipNone" ~: la ~=? loopChoiceSkip la,
    "lchoiceSkipLeafFirst" ~: 
        NodeN Seq [ploop la2 2.5 2,
                   NodeN Choice [lb,Silent 1] 2] 
              2 
                    ~=? loopChoiceSkip (choiceP [l4pa1,seqP [la,lb] 1] 2),
    "lchoiceSkipSeqFirst"  ~: 
        NodeN Seq [ploop la2 2.5 2,
                   NodeN Choice [lb,Silent 1] 2] 
              2 
                    ~=? loopChoiceSkip (NodeN Choice [seqP [l4pa1,lb] 1,la] 2),
    "lchoiceSkip3"    ~: NodeN Choice [ NodeN Seq [la,lc] 1,
                                      NodeN Seq [ploop la2 2.5 2,
                                                 NodeN Choice [lb,Silent 1] 
                                                               2] 2]
                              3 
                    ~=? loopChoiceSkip (choiceP [l4pa1,
                                                   seqP [la,lb] 1,
                                                   seqP [la,lc] 1] 
                                                   3),
    "lchoiceSkipWithTail" ~: seqP [ploop la4 2.5 4,
                                    choiceP [Silent 2,
                                             seqP[lb2,la2] 2 ] 4] 4
        ~=? loopChoiceSkip (NodeN Choice [ploop la2 4 2,
                                          seqP [la2,lb2,la2] 2.0] 4.0)
                    ]


choiceSkipTests = choiceSkipPrefixTests 
               ++ choiceSkipPrefixCompressTests 
               ++ loopChoiceSkipTests


fixedLoopRollTests = [
    "floopEq"    ~: la `floopContEq` Node1 FLoop la 2 1 ~=? True,
    "floopEq2"   ~: la == la ~=? True,
    "floopRoll1" ~: fixedLoopRoll la ~=? la,
    "floopRollSingle1" ~: Node1 FLoop la 2 1 ~=? fixedLoopRollSingle saa , 
    "floopRollSingle2" ~: Node1 FLoop la 2 1 ~=? fixedLoopRoll saa , 
    "floopRollExists3" ~: saa ~=? fixedLoopRollExisting saa,
    "floopRollExists4" ~: Node1 FLoop la 6 1 ~=?
            fixedLoopRollExisting (NodeN Seq [Node1 FLoop la 5 1,la] 1),
    "floopRoll3" ~: Node1 FLoop la 3 1 ~=? fixedLoopRoll saaa ,
    "floopRoll4" ~: Node1 FLoop la 4 1 
                        ~=? fixedLoopRoll (NodeN Seq [la,la,la,la] 1),
    "floopRollMid1" ~: saaat ~=? fixedLoopRoll saaat ,
    "floopRollMid2" ~: NodeN Seq [lb,Node1 FLoop la 3 1] 1 
                ~=? fixedLoopRoll (NodeN Seq [lb,la,la,la] 1),
    "floopRollPartial1" ~: Node1 FLoop la 6 1
                ~=? fixedLoopRoll (NodeN Seq [Node1 FLoop la 3 1,la,la,la] 1),
    "floopRollPartial2" ~: NodeN Seq [lb,Node1 FLoop la 5 1] 1 
                ~=? fixedLoopRoll (NodeN Seq [lb,Node1 FLoop la 3 1,la,la] 1),
    "floopRoll2" ~: NodeN Seq [la,lb,la,lb] 1
        ~=? fixedLoopRoll (NodeN Seq [la,lb,la,lb] 1),
    "floopRoll3a" ~: NodeN Seq [la,lb,lc,la,lb,lc] 1
         ~=? fixedLoopRoll (NodeN Seq [la,lb,lc,la,lb,lc] 1),
    "floopRoll3b" ~: NodeN Seq [la,lb,lc,la,lb,lc,la,lb,lc] 1
         ~=? fixedLoopRoll (NodeN Seq [la,lb,lc,la,lb,lc,la,lb,lc] 1)
        ]

probLoopRollTests = [
    "ploopRoll1" ~: la ~=? probLoopRoll la ,
    "ploopRoll2" ~: NodeN Seq [Node1 PLoop la 2 1] 1
                        ~=? probLoopRoll saa ,
    "ploopRoll3" ~: NodeN Seq [Node1 PLoop la 3 1] 1
                        ~=? probLoopRoll saaa ,
    "ploopRollMid1" ~: saaat ~=? probLoopRoll saaat ,
    "ploopRollMid2" ~: NodeN Seq [lb,Node1 PLoop la 3 1] 1 
                ~=? probLoopRoll (NodeN Seq [lb,la,la,la] 1),
    "ploopRollSim" ~: NodeN Seq [lb3,
                                 Node1 PLoop (NodeN Choice 
                                                    [Leaf "a" 1.5,
                                                     Leaf "b" 1.5] 3) 2 3,
                                 la3] 3
                ~=? probLoopRoll (NodeN Seq [lb3,NodeN Choice [la2,lb] 3,
                                                 NodeN Choice [la,lb2] 3, 
                                                la3] 3)
    ]

loopNestTests = [
    "loopNestFF1" ~: Node1 FLoop la5 6 5
        ~=? loopNest (Node1 FLoop (Node1 FLoop la5 2 5) 3 5),
    "loopNestFP1" ~: Node1 PLoop la5 6 5
        ~=? loopNest (Node1 FLoop (Node1 PLoop la5 2 5) 4 5),
    "loopNestPF1" ~: Node1 PLoop la5 40 5
        ~=? loopNest (Node1 PLoop (Node1 FLoop la5 10 5) 4 5),
    "loopNestPP1" ~: Node1 PLoop la5 30 5
        ~=? loopNest (Node1 PLoop (Node1 PLoop la5 10 5) 3 5)   ]

loopGeoTests = [
    "loopGeoNull" ~: la ~=? loopGeo la,
    "loopGeoSim" ~: Node1 PLoop lb2 3 2
        ~=? loopGeo (NodeN Choice [Node1 FLoop lb 2 1,
                                   Node1 FLoop lb 4 1] 2),
    "loopGeoId" ~: Node1 PLoop lb2 2 2
        ~=? loopGeo (NodeN Choice [Node1 FLoop lb 2 1,
                                   Node1 FLoop lb 2 1] 2),
    "loopGeoNe" ~: loopGeo (NodeN Choice [Node1 FLoop lb 2 1,
                                          Node1 FLoop la 4 1] 2)
        ~=? loopGeo (NodeN Choice [Node1 FLoop lb 2 1,
                                   Node1 FLoop la 4 1] 2),
    -- FLoop / PLoop merge depends on PLoop conversion 
    "loopGeo3" ~: NodeN Choice [Node1 PLoop lb2 3 2, Node1 FLoop lb 3 1] 3
        ~=? loopGeo (NodeN Choice [Node1 FLoop lb 2 1,
                                   Node1 FLoop lb 4 1,
                                   Node1 FLoop lb 3 1] 3),
    "loopGeoPartial" ~: choiceP [Node1 PLoop la2 3 2, lb] 3
        ~=? loopGeo (NodeN Choice [Node1 FLoop la 2 1,
                                   Node1 FLoop la 4 1,
                                   lb] 3),
    "loopGeoPartial2" ~: choiceP [Node1 PLoop la2 3 2, lb, lc] 4
        ~=? loopGeo (NodeN Choice [lc,
                                   Node1 FLoop la 2 1,
                                   Node1 FLoop la 4 1,
                                   lb] 4)
    ]


-- conc

concFromChoicePrefTests = [ 
    "concFromChoice1" ~: concFromChoice ccab1 ~=? ccab1,
    "concFromChoice2" ~: concFromChoice la  ~=? la,
    "concFromChoice3" ~: concP [la,lb] 2 
                            ~=? concFromChoice( NodeN Choice [sab,sba] 2),
    "concFromChoiceSeqNoop" ~: choiceP [sab,sab] 2
                            ~=? concFromChoice( NodeN Choice [sab,sab] 2),
    "concFromChoice4" ~: concP [la,lb2] 3 
                            ~=? concFromChoice( NodeN Choice [sab,sba2] 3) ,
    "concFromChoice5" ~: seqP [ concP [la,lb2] 3, choiceP [lc,Silent 2] 3 ] 3
                            ~=? concFromChoice( NodeN Choice [sabc,sba2] 3),
    "concFromChoice6" ~: seqP [ concP [la,lb2] 3, choiceP [lc,ld2] 3 ] 3
                            ~=? concFromChoice( NodeN Choice [sabc,sbad2] 3) 
                            ]

-- more len > 2 cases at different levels wouldn't hurt


tail2Tests = [ "empty" ~: ([]::[Int],[]::[Int]) ~=? tail2 ([]::[Int] ),
               "l1" ~: ([],[]) ~=? tail2 [1],
               "l2" ~: ([],[1,2]) ~=? tail2 [1,2],
               "l3" ~: ([1],[2,3]) ~=? tail2 [1,2,3],
               "l4" ~: ([4,5],[7,8]) ~=? tail2 [4,5,7,8] 
               ]

concFromChoiceSuffixTests = [ 
    "cfcLeaf"   ~: concFromChoiceSuffix la ~=? la,
    "concNoop"  ~: concFromChoiceSuffix ccab1 ~=? ccab1,
    "cfcs1" ~: concP [la,lb] 2 
                        ~=? concFromChoiceSuffix (NodeN Choice [sab,sba] 2),
    "concFromChoiceSeqNoop" ~: choiceP [sab,sab] 2
                            ~=? concFromChoice( NodeN Choice [sab,sab] 2),
    "cfcs2" ~: concP [la,lb2] 3 
                        ~=? concFromChoiceSuffix (NodeN Choice [sab,sba2] 3),
    "cfcs3" ~: seqP [ choiceP [la, Silent 2] 3, 
                      concP [lb,lc2] 3] 3 
                        ~=? concFromChoiceSuffix (NodeN Choice [sabc,scb2] 3),
    "cfcs4" ~: seqP [ choiceP [lc,ld2] 3,  concP [la,lb2 ] 3 ] 3
                        ~=? concFromChoiceSuffix (NodeN Choice [scab,sdba2] 3) 
    ]


loopConcFromChoicePrefTests = [ 
    "lconcFromChoice1" ~: ccab1 ~=? loopConcFromChoice ccab1, 
    "lconcFromChoice2" ~: la ~=? loopConcFromChoice la ,
    "lconcFromChoice3" ~: concP [ploop la 2.5 1,lb] 2 
                        ~=? loopConcFromChoice( NodeN Choice [slab,sba] 2),
    "lconcFromChoiceSeqNoop" ~: choiceP [sab,sab] 2
                        ~=? loopConcFromChoice( NodeN Choice [sab,sab] 2),
    "lconcFromChoice4" ~: concP [ploop la 2 1,lb2] 3 
                        ~=? loopConcFromChoice( NodeN Choice [slab,sba2] 3) ,
    "lconcFromChoice5" ~: seqP [concP [ploop la 2 1,lb2] 3, 
                                choiceP [lc,Silent 2] 3 ] 3
                        ~=? loopConcFromChoice( NodeN Choice [slabc,sba2] 3),
    "lconcFromChoice6" ~: seqP [concP [ploop la 2 1,lb2] 3, 
                                choiceP [lc,ld2] 3 ] 3
                        ~=? loopConcFromChoice( NodeN Choice [slabc,sbad2] 3),
    "lconcFromChoiceTwoLoops" ~: 
        concP [ploop la (16/3) 1,ploop lb2 (7/3) 2] 3 
            ~=? loopConcFromChoice( NodeN Choice [slab,slbla2] 3) 
                        ]


loopConcFromChoiceSuffixTests = [ 
    "lcfcLeaf"   ~: loopConcFromChoiceSuffix la ~=? la,
    "lconcNoop"  ~: loopConcFromChoiceSuffix ccab1 ~=? ccab1,
    "lcfcs1" ~: concP [ploop la 2.5 1,lb] 2 
                    ~=? loopConcFromChoiceSuffix (NodeN Choice [slab,sba] 2),
    "lconcFromChoiceSeqNoop" ~: choiceP [slab,sab] 2
                            ~=? loopConcFromChoice( choiceP [slab,sab] 2),
    "lcfcs2" ~: concP [ploop la 2 1,lb2] 3 
                    ~=? loopConcFromChoiceSuffix (NodeN Choice [slab,sba2] 3),
    "lcfcs3" ~: seqP [choiceP [la, Silent 2] 3, 
                      concP [lb,ploop lc2 2 2] 3] 3 
                    ~=? loopConcFromChoiceSuffix (NodeN Choice [sablc,scb2] 3),
    "lcfcs4" ~: seqP [ choiceP [lc,ld2] 3,  concP [la,ploop lb2 3 2] 3 ] 3
                    ~=? loopConcFromChoiceSuffix (NodeN Choice [scab,sdlba2] 3) 
    ]

concFromChoiceTests = concFromChoicePrefTests
                   ++ tail2Tests
                   ++ concFromChoiceSuffixTests
                   ++ loopConcFromChoicePrefTests
                   ++ loopConcFromChoiceSuffixTests

concSubsumeTests1 = [
    "concSubsume1" ~: concSubsume la ~=? la,
    "concSubsume2" ~: cab ~=? concSubsume cab,
    "concSubsume3" ~: choiceP [seqP [la,lb] 1,
                               seqP [la,lb] 1] 2
        ~=?  concSubsume (choiceP [seqP [la,lb] 1,
                                   seqP [la,lb] 1] 2),
    "concSeqSim1"  ~: False ~=? concSeqSim la lb,
    "concSubsumePair1" ~: NodeN Conc [la2,lb] 3
                ~=? concSubsume (NodeN Choice [NodeN Seq  [la,lb] 1,
                                               NodeN Conc [la,lb] 2] 3),
    "concSubsumePair2" ~: NodeN Conc [la3,lb] 4
                ~=? concSubsume (NodeN Choice [NodeN Conc [la,lb] 2,
                                               NodeN Seq  [la2,lb2] 2] 4),
    "concSubsumePair3" ~: NodeN Conc [la4,lb] 5
                ~=? concSubsume (NodeN Choice [NodeN Conc [la2,lb] 3,
                                               NodeN Seq  [la2,lb2] 2] 5),
    "concSubsumePair4" ~: NodeN Conc [la2,lb3] 5
                ~=? concSubsume (NodeN Choice [NodeN Conc [la2,lb] 3,
                                               NodeN Seq  [lb2,la2] 2] 5),
    "concSubsumePairVarLengths" ~: 
            seqP [ NodeN Conc [la2,lb3] 5, choiceP [Silent 3, lc2] 5 ] 5
                ~=? concSubsume (NodeN Choice [NodeN Conc [la2,lb] 3,
                                               NodeN Seq  [lb2,la2,lc2] 2] 5),
    "concSubsumeTriple1" ~: NodeN Conc [la2,lb,lc] 3
                ~=? concSubsume (NodeN Choice [NodeN Conc [la,lc,lb] 2,
                                               NodeN Seq  [la,lb,lc] 1] 4)
                                              ]


loopConcSubsumeFwdTests = [
    "lconcSubsume1" ~: loopConcSubsumeFwd la ~=? la,
    "lconcSubsume2" ~: cab ~=? loopConcSubsumeFwd cab,
    "lconcSubsume3" ~: choiceP [seqP [ploop la 4 1,lb] 1,
                                seqP [la,lb] 1] 2
        ~=? loopConcSubsumeFwd (choiceP [seqP [ploop la 4 1,lb] 1,
                                   seqP [la,lb] 1] 2),
    "lconcSubsumePair1" ~: concP [ploop la2 2.5 2,lb] 3
        ~=? loopConcSubsumeFwd (NodeN Choice [NodeN Seq  [ploop la 4 1,lb] 1,
                                           NodeN Conc [la,lb] 2] 3),
    "lconcSubsumePair2" ~: concP [ploop la3 3 3,lb] 4
        ~=? loopConcSubsumeFwd (NodeN Choice [concP [la,lb] 2,
                                           seqP  [ploop la2 4 2,lb2] 2] 4),
    "lconcSubsumePair3" ~: concP [ploop la4 2 4,lb] 5
        ~=? loopConcSubsumeFwd (NodeN Choice [NodeN Conc [ploop la2 3 2,lb] 3,
                                           NodeN Seq  [la2,lb2] 2] 5),
    "lconcSubsumeTail1" ~: seqP[concP [ploop la4 2.5 4,lb] 5,
                                choiceP [Silent 3,lc2] 5 ] 5  
        ~=? loopConcSubsumeFwd (choiceP [concP [la2,lb] 3,
                                      seqP [ploop la2 4 2,lb2,lc2] 2] 5),
    "lconcSubsumePairRevNoop" ~: 
        choiceP [concP [la2,lb] 3,
                 seqP  [lb2,ploop la2 7 2] 2] 5
        ~=? loopConcSubsumeFwd (choiceP [concP [la2,lb] 3,
                                      seqP  [lb2,ploop la2 7 2] 2] 5),
    "lconcSubsumeTripleNoop" ~: 
            NodeN Choice [NodeN Conc [ploop la 4 1,lc,lb] 2,
                          NodeN Seq  [la,lb,lc] 1] 4
        ~=? loopConcSubsumeFwd 
                (NodeN Choice [NodeN Conc [ploop la 4 1,lc,lb] 2,
                               NodeN Seq  [la,lb,lc] 1] 4)
         ]


loopConcSubsumeRevTests = [
    "lconcSubsume1" ~: loopConcSubsumeRev la ~=? la,
    "lconcSubsume2" ~: cab ~=? loopConcSubsumeRev cab,
    "lconcSubsume3" ~: choiceP [seqP [ploop la 4 1,lb] 1,
                                seqP [la,lb] 1] 2
        ~=? loopConcSubsumeRev (choiceP [seqP [ploop la 4 1,lb] 1,
                                   seqP [la,lb] 1] 2),
    "lconcSubsumePair1" ~: concP [ploop la2 2.5 2,lb] 3
        ~=? loopConcSubsumeRev (NodeN Choice [NodeN Seq  [ploop la 4 1,lb] 1,
                                              NodeN Conc [lb,la] 2] 3),
    "lconcSubsumePair2" ~: concP [ploop la 3 1,lb3] 4
        ~=? loopConcSubsumeRev (NodeN Choice [concP [la,lb] 2,
                                              seqP  [lb2, ploop la2 4 2] 2] 4),
    "lconcSubsumePair3" ~: concP [ploop la2 2 2,lb3] 5
        ~=? loopConcSubsumeRev (NodeN Choice [NodeN Conc [ploop la2 3 2,lb] 3,
                                              NodeN Seq  [lb2,la2] 2] 5),
    "lconcSubsumeTail1" ~: seqP[concP   [ploop la2 2.5 2,lb3] 5,
                                choiceP [Silent 3,lc2] 5 ] 5  
        ~=? loopConcSubsumeRev (choiceP [concP [la2,lb] 3,
                                         seqP  [lb2,ploop la2 4 2,lc2] 2] 5),
    "lconcSubsumePairFwdNoop" ~: 
        choiceP [concP [la2,lb] 3,
                 seqP  [ploop la2 7 2,lb2] 2] 5
        ~=? loopConcSubsumeRev (choiceP [concP [la2,lb] 3,
                                         seqP  [ploop la2 7 2,lb2] 2] 5),
    "lconcSubsumeTripleNoop" ~: 
            NodeN Choice [NodeN Conc [ploop la 4 1,lc,lb] 2,
                          NodeN Seq  [la,lb,lc] 1] 4
        ~=? loopConcSubsumeRev 
                (NodeN Choice [NodeN Conc [ploop la 4 1,lc,lb] 2,
                               NodeN Seq  [la,lb,lc] 1] 4)
         ]


concSubsumeTests = concSubsumeTests1 
                ++ loopConcSubsumeFwdTests
                ++ loopConcSubsumeRevTests

--

flattenTests = [
    "leaf"        ~: la ~=? flatten la,
    "choiceNoop"  ~: NodeN Choice [la,NodeN Seq [lb,lc] 2] 3
                  ~=? flatten (NodeN Choice [la,NodeN Seq [lb,lc] 2] 3),
    "choiceFlatten" ~: NodeN Choice [la,lb,lc] 3 
                  ~=? flatten (NodeN Choice [la, NodeN Choice [lb,lc] 2] 3 ) ,
    "seqFlatten" ~: NodeN Seq [la,lb,lc] 3 
                   ~=? flatten (NodeN Seq [la, NodeN Seq [lb,lc] 2] 3 ),
    "mixFlatten" ~: NodeN Choice [la,lb,lc,ld] 4 
                  ~=? flatten (NodeN Choice [la, NodeN Choice [lb,lc] 2, ld] 4),
    "compound"  ~: NodeN Choice [NodeN Seq [la,lb] 1,
                                 NodeN Seq [lb,ld] 1,
                                 NodeN Seq [ld,lb] 1] 3
                  ~=? flatten ( NodeN Choice 
                                    [NodeN Choice 
                                           [NodeN Seq [la,lb] 1,
                                           NodeN Seq [lb,ld] 1] 2, 
                                    NodeN Seq [ld,lb] 1] 3) ]
               
--
choicePruneTests = [
    "cpNoop" ~: la ~=? choicePrune la 0.1,
    "cpNoop2" ~:  choiceP [la,lb2] 3 ~=? choicePrune (choiceP [la,lb2 ] 3 ) 0.1,
    "cpPruneAll" ~: Silent 3 ~=? choicePrune (choiceP [la,lb,lc ] 3 ) 0.5,
    "cpPruneToLeaf"   ~: la3 ~=? choicePrune (choiceP [la2,lb] 3) 0.5,
    "cpPrune1"   ~: True ~=?
        choiceP [Leaf "b" (12/5),Leaf "c" (18/5) ] 6
        `acmp` choicePrune (choiceP [la,lb2,lc3] 6) (1/4),
    "cpPrune2"   ~: True ~=?
        choiceP [Leaf "b" (20/9), Leaf "c" (30/9),
                             Leaf "d" (40/9)] 10
        `acmp` choicePrune (choiceP [la,lb2,lc3,ld4] 10) 0.2,
    "cpPrune2"   ~: True ~=?
        choiceP [Leaf "c" (30/7), Leaf "d" (40/7)] 10
        `acmp` choicePrune (choiceP [la,lb2,lc3,ld4] 10) 0.3
    ]




--

transformInOrderSimpleTests = [
    "transformRONoop" ~: la ~=? ssRuleOrdered la,
    "transformROsilent" ~: NodeN Seq [lb2,la2] 2 
                        ~=? ssRuleOrdered (NodeN Seq [lb2,Silent 2,la2] 2),
    "transformROSilentSingle" ~: NodeN Seq [lb2,la2] 2
            ~=? ssRuleOrdered (NodeN Seq [lb2,NodeN Seq [la2,Silent 2] 2] 2)]

--

comp1 = NodeN Seq [Node1 PLoop
                        (Leaf "Eat Chocolate" 5.0)
                        1.8 5.0,
                   NodeN Choice
                        [NodeN Seq
                            [Leaf "Clean Teeth" 2.0,
                             Leaf "Grow Fat" 2.0]
                            2.0,
                         NodeN Seq
                            [Leaf "Grow Fat" 3.0,
                             Leaf "Clean Teeth" 3.0]
                            3.0]
                        5.0]
              5.0


validateTests = [
    "validSeq"   ~: validate (NodeN Seq [la,lb,lc] 1) @? "val",
    "invalidSeqWeight" ~: not ( validate (NodeN Seq [la,lb2] 4) ) @? "inval" ,
    "validCompound1" ~: validate comp1 @? "val",
    "validCompound2" ~:
        validate ( NodeN Choice
                       [NodeN Seq
                            [Node1 PLoop
                                (Leaf "Eat Chocolate" 3.0) 1.3333334 3.0,
                             NodeN Seq
                                [Leaf "Grow Fat" 3.0,
                                 Leaf "Clean Teeth" 3.0]
                                3.0]
                            3.0,
                        NodeN Seq
                            [Node1 PLoop
                                (Leaf "Eat Chocolate" 2.0) 2.5 2.0,
                             NodeN Seq [Leaf "Clean Teeth" 2.0,
                                        Leaf "Grow Fat" 2.0] 
                                   2.0 ] 
                              2.0 ] 
                        5.0) @? "val"
                    ]


--

metaRuleTests = adjMergeTests ++ anyMergeTests

ruleTests   = silentSeqTests  ++ silentConcTests
           ++ singleNodeOpTests 
           ++ choiceSimTests ++ loopSimTests
           ++ concSimTests ++ loopConcSimTests
           ++ choiceFoldPrefixTests
           ++ choiceFoldSuffixTests 
           ++ choiceSkipTests 
           ++ concFromChoiceTests
           ++ concSubsumeTests
           ++ fixedLoopRollTests ++ loopNestTests ++ loopGeoTests
           ++ probLoopRollTests 
           ++ loopChoiceFoldTests
           ++ flattenTests
           ++ choicePruneTests

transformTests = transformInOrderSimpleTests

huTests     = eqTests
            ++ transformTests
            ++ ruleTests
            ++ validateTests
            ++ metaRuleTests


