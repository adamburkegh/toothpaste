module Toothpaste.OtherRulesTest where

import Toothpaste.OtherRules
import Toothpaste.ProbProcessTree

import Test.HUnit


la   = Leaf "a" 1
la2  = Leaf "a" 2
la3  = Leaf "a" 3
lb   = Leaf "b" 1
lb2  = Leaf "b" 2
lb3 = Leaf "b" 3

lc   = Leaf "c" 1
lc3  = Leaf "c" 3
ld4  = Leaf "d" 4

saa = NodeN Seq [la,la] 1
saaa = NodeN Seq [la,la,la] 1
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



huTests     = choiceSkipSuffixTests
            ++ fixedLoopRollTestsNSingle ++ fixedLoopRollTestsNforN
            ++ probLoopRollTests


