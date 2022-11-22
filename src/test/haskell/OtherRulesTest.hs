module OtherRulesTest where

import OtherRules
import ProbProcessTree

import PPTTestUtil

import Test.HUnit


la   = Leaf "a" 1
la2  = Leaf "a" 2
la3  = Leaf "a" 3
lb   = Leaf "b" 1
lb2  = Leaf "b" 2
lc   = Leaf "c" 1
lc3  = Leaf "c" 3
ld4  = Leaf "d" 4

saaat = NodeN Seq [la,NodeN Seq [la,la] 1] 1



-- not a betatrap rule
choiceSkipSuffixTests = [
    "choiceSkipNone" ~: la ~=? choiceSkipSuffix la,
    "choiceSkipLeafLast" ~:
        NodeN Seq [ NodeN Choice [lb,Silent 1] 2,
                    la2]
              2
                    ~=? choiceSkipSuffix (NodeN Choice [la,seqP [lb,la] 1] 2),
    "choiceSkipSeqLast"  ~:
        NodeN Seq [ NodeN Choice [lb,Silent 1] 2,
                    la2]
              2
                    ~=? choiceSkipSuffix (NodeN Choice [seqP [lb,la] 1,la] 2),
    "choiceSkip3"    ~: NodeN Choice [NodeN Seq [lc,la] 1,
                                      NodeN Seq [NodeN Choice [lb,Silent 1] 2,
                                                 la2] 2]
                              3
                    ~=? choiceSkipSuffix (choiceP [la,
                                                   seqP [lb,la] 1,
                                                   seqP [lc,la] 1]
                                                   3)
                    ]

fixedLoopRollTestsNSingle = [
    "fixedLoopRollListN1No" ~: [la,lc,lb] ~=? fixedLoopRollListN [lc,lb] [la] 1,
    "fixedLoopRollListN1Yes" ~: [Node1 FLoop la 4 1]
            ~=? fixedLoopRollListN [la,la,la] [la] 1,
    "fixedLoopRollListN3No" ~: [la,lb,lc,la,lb]
            ~=? fixedLoopRollListN [la,lb] [la,lb,lc] 1,
    "fixedLoopRollListN3Yes" ~: [Node1 FLoop (NodeN Seq [la,lb,lc] 1) 7 1]
            ~=? fixedLoopRollListN [la,lb,lc] [la,lb,lc] 6
        ]


fixedLoopRollTestsNforN = [
    "floopRoll1" ~: fixedLoopRollLengthN la ~=? la,
    "floopRoll2" ~: Node1 FLoop (NodeN Seq [la,lb] 1) 2 1
        ~=? fixedLoopRollLengthN (NodeN Seq [la,lb,la,lb] 1),
    "floopRoll3a" ~: Node1 FLoop (NodeN Seq [la,lb,lc] 1) 2 1
         ~=? fixedLoopRollLengthN (NodeN Seq [la,lb,lc,la,lb,lc] 1),
    "floopRoll3b" ~: Node1 FLoop (NodeN Seq [la,lb,lc] 1) 3 1
         ~=? fixedLoopRollLengthN (NodeN Seq [la,lb,lc,la,lb,lc,la,lb,lc] 1),
    "floopRollMid1" ~: saaat ~=? fixedLoopRollLengthN saaat ,
    -- interesting edge case: no simplification if loop introduced
    "floopRollMidNoop" ~: NodeN Seq [lc,la,lb,la,lb] 1
                ~=? fixedLoopRollLengthN (NodeN Seq [lc,la,lb,la,lb] 1),
    "floopRollMid3" ~: NodeN Seq [lc,Node1 FLoop (NodeN Seq [la,lb] 1) 3 1] 1
                ~=? fixedLoopRollLengthN (NodeN Seq [lc,la,lb,la,lb,la,lb] 1),
    "fixedLoopRollListN1No" ~: [la,lc,lb] ~=? fixedLoopRollListN [lc,lb] [la] 1,
    "fixedLoopRollListN1Yes" ~: [Node1 FLoop la 4 1]
            ~=? fixedLoopRollListN [la,la,la] [la] 1,
    "fixedLoopRollListN3No" ~: [la,lb,lc,la,lb]
            ~=? fixedLoopRollListN [la,lb] [la,lb,lc] 1,
    "fixedLoopRollListN3Yes" ~: [Node1 FLoop (NodeN Seq [la,lb,lc] 1) 7 1]
            ~=? fixedLoopRollListN [la,lb,lc] [la,lb,lc] 6
        ]

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


huTests     = choiceSkipSuffixTests
            ++ fixedLoopRollTestsNSingle ++ fixedLoopRollTestsNforN
            ++ choicePruneTests 

