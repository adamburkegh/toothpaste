module OtherRulesTest where

import OtherRules
import ProbProcessTree

import Test.HUnit

la   = Leaf "a" 1
la2  = Leaf "a" 2
lb   = Leaf "b" 1
lc   = Leaf "c" 1


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

huTests     = choiceSkipSuffixTests

