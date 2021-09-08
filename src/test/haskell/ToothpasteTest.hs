module ToothpasteTest where

import Toothpaste hiding (main)

import Data.List (sort)
import Data.Set (Set,toList,fromList)
import System.Exit
import Test.HUnit



la = Leaf "a" 1
lb = Leaf "b" 1
lc = Leaf "c" 1
ld = Leaf "d" 1
le = Leaf "e" 1
lf = Leaf "f" 1
lg = Leaf "g" 1
cab = NodeN Choice [la,lb] 2
cab2 = NodeN Choice [NodeN Seq [lb,la] 1,NodeN Seq [lb,la] 1] 2
cab3 = NodeN Choice [NodeN Seq [lb,la] 1,NodeN Seq [lb,lb] 1] 2
cab4 = NodeN Choice [NodeN Seq [lb,la,lc] 1,NodeN Seq [lb,lb,lb] 1] 2
cabc1 = NodeN Choice [la,lb,lc] 1
saa = NodeN Seq [la,la] 1
saaa = NodeN Seq [la,la,la] 1
saaat = NodeN Seq [la,NodeN Seq [la,la] 1] 1


sab = NodeN Seq [la,lb] 1
sba = NodeN Seq [lb,la] 1

ccab1 = NodeN Conc [la,lb] 2

la2 = Leaf "a" 2
la3 = Leaf "a" 3
la4 = Leaf "a" 4
la11 = Leaf "a" 11
lb2 = Leaf "b" 2
lb3 = Leaf "b" 3
lc2 = Leaf "c" 2
lc3 = Leaf "c" 3
ld2 = Leaf "d" 2
loopa1 = Node1 FLoop la 1 1

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
           "eqNode2" ~: cab /= cab2 @? "neq"]

-- Rule tests

silentSeqTests = [
    "silentSeqNoop" ~: ccab1 ~=? silentSeq ccab1,
    "silentSeq2Seq" ~: NodeN Seq [la] 1
                        ~=? silentSeq (NodeN Seq [la,Silent 1] 1),
    "silentSeq3Seq" ~: NodeN Seq [la,lb] 1 
                        ~=? silentSeq (NodeN Seq [la,Silent 1,lb] 1)  ]

singleNodeOpTests = [ 
    "singleNodeNoop"  ~: cab2 ~=? singleNodeOp cab2 ,
    "singleNodeOpSeq" ~: la ~=? singleNodeOp (NodeN Seq [la] 1)  ]

choiceSimTests = ["choiceSim1" ~: NodeN Choice [la2] 2 
                        ~=? choiceSim(  NodeN Choice [la,la] 2),
                 "choiceSim2" ~: cab ~=? choiceSim cab ,
                 "choiceSim3" ~: NodeN Choice [NodeN Seq [lb2,la2] 2] 2 
                        ~=? choiceSim cab2,
                 "choiceSim4" ~: cab3 ~=? choiceSim cab3 ,
                 "choiceSimLoop" ~: NodeN Choice [Node1 PLoop la2 6 2] 2
                    ~=? choiceSim (NodeN Choice [Node1 PLoop la 7 1,
                                                 Node1 PLoop la 5 1] 2) ]

concSimTests = [
    "concSim1" ~: la ~=? concSim la,
    "concSim2" ~: NodeN Conc [Node1 FLoop la2 2 2] 2 
                        ~=? concSim(NodeN Conc [la,la] 2) ,
    "concSim3" ~: NodeN Conc [la,lb] 2 ~=? concSim(NodeN Conc [la,lb] 2)  
                ]

choiceFoldPrefixTests = [
    "choiceFold1" ~: choiceFoldPrefix la ~=? la,
    "choiceFold2" ~: choiceFoldPrefix cab  ~=? cab,
    "choiceFoldEmptyTailSeq" ~: 
        NodeN Choice [NodeN Seq [lb2, NodeN Choice [NodeN Seq [la] 1,
                                                    NodeN Seq [la] 1] 2] 
                             2] 2 
                            ~=? choiceFoldPrefix cab2 ,
    "choiceFoldLeftoverSeq" ~: 
        NodeN Choice [NodeN Seq [lb2,
                      NodeN Choice [NodeN Seq [la,lc] 1,
                                    NodeN Seq [lb,lb] 1] 2] 
              2] 2 
                            ~=? choiceFoldPrefix cab4 ,
    "choiceFoldPrefixMore4Choices" ~:
        NodeN Choice [NodeN Seq [la,lb] 1,
                      NodeN Seq [lb2, 
                                 NodeN Choice [NodeN Seq [lc] 1,
                                               NodeN Seq [ld] 1] 2] 2,
                      NodeN Seq [la,lb] 1] 4
            ~=? choiceFoldPrefix (NodeN Choice [NodeN Seq [la,lb] 1,
                                               NodeN Seq [lb,lc] 1,
                                               NodeN Seq [lb,ld] 1,
                                               NodeN Seq [la,lb] 1] 4),
    "choiceFoldId1" ~: choiceSim (choiceFoldPrefix cab2) /= 
                            choiceFoldPrefix (choiceSim cab2) @? "neq" ]

{-
    "choiceFoldLongSuffix" ~:
           Node2 Seq (NodeN Choice (Node2 Seq la
                                              (Node2 Seq lb lc 1) 1)
                                   (Node2 Seq le
                                              (Node2 Seq lf lg 1) 1)
                                   2)
                      ld2 2
                            ~=? choiceFold (Node2 Choice sabcd sefgd 2)  -}

fixedLoopRollTests = [
    "floopRoll1" ~: fixedLoopRoll la ~=? la,
    "floopRoll2" ~: Node1 FLoop la 2 1 ~=? fixedLoopRoll saa , 
    "floopRoll3" ~: Node1 FLoop la 3 1
                            ~=? fixedLoopRoll saaa ,
    "floopRollMid1" ~: saaat ~=? fixedLoopRoll saaat   ]

loopNestTests = [
    "loopNestFF1" ~: Node1 FLoop la 6 5
        ~=? loopNest (Node1 FLoop (Node1 FLoop la 2 5) 3 5),
    "loopNestFP1" ~: Node1 PLoop la 6 5
        ~=? loopNest (Node1 FLoop (Node1 PLoop la 2 5) 4 5),
    "loopNestPF1" ~: Node1 PLoop la 40 5
        ~=? loopNest (Node1 PLoop (Node1 FLoop la 10 5) 4 5),
    "loopNestPP1" ~: Node1 PLoop la 30 5
        ~=? loopNest (Node1 PLoop (Node1 PLoop la 10 5) 3 5)   ]

loopGeoTests = [
    "loopGeoNull" ~: la ~=? loopGeo la,
    "loopGeoSim" ~: NodeN Choice [Node1 PLoop lb2 3 2] 2
        ~=? loopGeo (NodeN Choice [Node1 FLoop lb 2 1,
                                   Node1 FLoop lb 4 1] 2),
    "loopGeoId" ~: NodeN Choice [Node1 PLoop lb2 2 2] 2
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
    "loopGeoPartial" ~: NodeN Choice [Node1 PLoop la2 3 2, lb] 3
        ~=? loopGeo (NodeN Choice [Node1 FLoop la 2 1,
                                   Node1 FLoop la 4 1,
                                   lb] 3)
    ]


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

transformInOrderSimpleTests = [
    "transformRONoop" ~: la ~=? ssRuleOrdered la,
    "transformROsilent" ~: NodeN Seq [lb2,la2] 2 
                        ~=? ssRuleOrdered (NodeN Seq [lb2,Silent 2,la2] 2),
    "transformROSilentSingle" ~: NodeN Seq [lb2,la2] 2
            ~=? ssRuleOrdered (NodeN Seq [lb2,NodeN Seq [la2,Silent 2] 2] 2)]



--

ruleTests   = silentSeqTests ++ singleNodeOpTests 
           ++ choiceSimTests ++ concSimTests
           ++ choiceFoldPrefixTests
           ++ fixedLoopRollTests ++ loopNestTests ++ loopGeoTests
           ++ flattenTests

transformTests = transformInOrderSimpleTests

huTests     = eqTests
            ++ transformTests
            ++ ruleTests


