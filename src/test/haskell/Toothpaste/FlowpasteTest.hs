module Toothpaste.FlowpasteTest where

import Toothpaste.Flowpaste hiding (main)
import Toothpaste.PetriNet
import Toothpaste.EventLog
import Data.Set (Set,toList,fromList)

import System.Exit
import Test.HUnit

-- concrete examples

sptree x = traceModel $ parseDelimitedTrace x

choicet :: (Ord a) => [PTree a] -> PTree a
choicet [x] = x
choicet (x:xs) = Node2 Choice x (choice xs)


la = Leaf "a"
lb = Leaf "b"
lc = Leaf "c"
ld = Leaf "d"
le = Leaf "e"
lf = Leaf "f"
lg = Leaf "g"
cab = Node2 Choice la lb
cab2 = Node2 Choice (Node2 Seq lb la) (Node2 Seq lb la )
cab3 = Node2 Choice (Node2 Seq lb la ) (Node2 Seq lb lb )
cabc1 = Node2 Choice (Node2 Seq lb la ) (Node2 Seq lb lc )
saa = Node2 Seq la la
saaa = Node2 Seq la (Node2 Seq la la)
saaab =  sptree "a a a b"
sabcd = sptree "a b c d"
sefgd = sptree "e f g d"
lab = sptree "a b a b"
sab = Node2 Seq la lb
sba = Node2 Seq lb la
ccab1 = Node2 Conc la lb


-- tests

eqTests = ["eqLeaf1Test" ~: Leaf "a" ~=? Leaf "a",
           "eqLeaf2Test" ~: Leaf "a" /= Leaf "b" @? "neq",
           "eqLeaf3Test" ~: Leaf "a" /= cab @? "neq",
           "eqNode1Test" ~: cab ~=? cab,
           "eqNode2Test" ~: cab /= cab2 @? "neq"]

choiceIdTests = ["choiceId1Test" ~: choiceId(  Node2 Choice la la ) ~=? la,
                 "choiceId2Test" ~: choiceId cab  ~=? cab,
                 "choiceId3Test" ~: choiceId cab2 ~=? Node2 Seq lb la,
                 "choiceId4Test" ~: choiceId cab3 ~=? cab3 ]

-- consider a commute normalizer for testing?
commutChoiceId = trule (commutTRule Choice choiceId "choiceId")

commutChoiceTests = [ "cchoiceId1Test" ~: 
                        commutChoiceId (choice[la,lb,lc,ld,le,la]) ~=?
                            Node2 Choice 
                                (Node2 Choice 
                                    (Node2 Choice 
                                             (Node2 Choice la le) 
                                             ld) 
                                    lc) 
                                lb,
                      "cchoiceId2Test" ~: 
                        commutChoiceId (choice [la,lb,lc, la, Node2 Seq la lc])
                        ~=? Node2 Choice 
                                (Node2 Choice la 
                                             (Node2 Choice lc 
                                                          (Node2 Seq la lc))) 
                                lb,
                      "cchoiceId3Test" ~: 
                      commutChoiceId (choicet [la,lb,lc,la]) ~=?
                            Node2 Choice (Node2 Choice la lc) lb ]

commutConcChoice = trule (commutTRule Choice concFromChoice "concFromChoice")

cc2x = Node2 Choice (Node2 Choice sba (Node2 Choice (Node1 Loop lb) sab)) 
                    (Node1 Loop la)

commutConcTests = [ "cconcChoice1Test" ~: 
                    commutConcChoice (choicet [ sab, la, la, la, sba, lb]) ~=?
                        Node2 Choice 
                            (Node2 Choice 
                                (Node2 Choice 
                                    (Node2 Choice ccab1 lb) 
                                    la ) 
                                la )
                            la ,
                    "cconcChoice2Test" ~: commutConcChoice cc2x ~=?
                        Node2 Choice (Node2 Choice ccab1 (Node1 Loop lb)) 
                                     (Node1 Loop la),
                    "cconcChoice3Test" ~: 
                        commutConcChoice( Node2 Choice sba 
                                                (Node2 Choice la sab))
                                ~=? Node2 Choice ccab1 la ]

choiceFoldTests = ["choiceFold1Test" ~: choiceFold la ~=? la,
                   "choiceFold2Test" ~: choiceFold cab  ~=? cab,
                   "choiceFold3Test" ~: choiceFold cab2 ~=? 
                                Node2 Seq lb (Node2 Choice la la),
                   "choiceFoldId1Test" ~: 
                    choiceId (choiceFold cab2) /= choiceFold (choiceId cab2) 
                                @? "neq",
                   "choiceFoldLongSuffix" ~:  
                           Node2 Seq (Node2 Choice (Node2 Seq la 
                                                            (Node2 Seq lb lc)) 
                                                   (Node2 Seq le  
                                                            (Node2 Seq lf lg)))
                                 ld 
                            ~=? choiceFold (Node2 Choice sabcd sefgd) ]


loopRollTests = ["loopRoll1Test" ~: loopRoll la ~=? la,
                 "loopRoll2Test" ~: loopRoll saa  ~=? Node1 Loop la,
                 "loopRollMid1Test" ~: loopRoll saaa ~=? 
                            Node2 Seq (Node1 Loop la) la ]

loopNestTests = ["loopNest1Test" ~: loopNest (Node1 Loop (Node1 Loop la)) 
                                    ~=? Node1 Loop la ]

concIdTests = [ "concId1Test" ~: concId la ~=? la,
                "concId2Test" ~: concId(Node2 Conc la la) ~=? Node1 Loop la,
                "concId3Test" ~: concId(Node2 Conc la lb) ~=? Node2 Conc la lb]

concFromChoiceTests = [ 
    "concFromChoice1Test" ~: concFromChoice ccab1 ~=? ccab1,
    "concFromChoice2Test" ~: concFromChoice la  ~=? la,
    "concFromChoice3Test" ~: concFromChoice( Node2 Choice sab sba) 
                                ~=? Node2 Conc la lb ]

concSubsumeTests = [
    "concSubsume1Test" ~: concSubsume la ~=? la,
    "concSubsume2Test" ~: concSubsume (Node2 Choice (Node2 Seq la lb) 
                                             (Node2 Conc la lb) )
                             ~=? Node2 Conc la lb
    ]

silentSeqTests = [
    "silentSeq1Test" ~: silentSeq ccab1 ~=? ccab1,
    "silentSeq2Test" ~: silentSeq (Node2 Seq la Silent) ~=? la ]

nlr = longSeq (Node2 Seq la 
                    (Node2 Seq la 
                          (Node2 Seq lb 
                                    (Node2 Seq la 
                                            (Node2 Seq (Node1 Loop 
                                                                (Node2 Seq lb 
                                                                           la)) 
                                                        lc)))) ) 

longLoopTests = [
    "prefix1" ~: prefix la saaa @? "prefix",
    "prefix2" ~: not (prefix lb saaa) @? "not prefix",
    "rmpref1" ~: rmpref la lb ~=? lb,
    "rmpref2" ~: rmpref la saaa ~=? Silent,
    "rmpref3" ~: rmpref la saaab ~=? lb,
    "longSeq1" ~: longSeq lb ~=? lb,
    "longSeq2" ~: longSeq lab ~=? 
            Node1 Loop (Node2 Seq la lb) ,
    "longSeq3" ~: longSeq nlr ~=?  nlr ]


transformTests = [
    "transform1Test" ~: transform la ~=? la,
    "transform2Test" ~: transform cab2 ~=? Node2 Seq lb la,
    "transform3Test" ~: transform cab3 ~=? Node2 Choice
                                            (Node2 Seq lb la)
                                            (Node1 Loop lb),
    "transform4Test" ~: transform cabc1 ~=? Node2 Seq lb
                                               (Node2 Choice la lc ) ]

--

termSeq x = Node2 Seq x terminal

traceTests = [
    "tracePTree1Test" ~: tracePTree ["a"] ~=? la,
    "tracePTree2Test" ~: tracePTree ["a","b"] ~=? Node2 Seq la lb,
    "tracePTree3Test" ~: tracePTree ["a","b","c"] ~=?
                            Node2 Seq la (Node2 Seq lb lc ), 
    "traceModel1Test" ~: traceModelT [["a"]] ~=? termSeq la,
    "traceModel2Test" ~: traceModelT [["a","b","c"]] ~=?
                            Node2 Seq la (Node2 Seq lb (termSeq lc) ),
    "traceModel3Test" ~: traceModelT [["a","b","c"],["a","b"]] ~=?
                            Node2 Choice
                                (Node2 Seq la lb) 
                                (Node2 Seq
                                    la
                                    (Node2 Seq lb 
                                              (termSeq lc)  ) )
        ]

discDL = discover parseDelimitedTrace

discoverTests = [
    "disco1" ~: discDL "a" ~=? la,
    "disco2" ~: discDL "a b c\na b" ~=?
                  Node2 Seq la 
                            (Node2 Seq lb
                                      (Node2 Choice lc Silent) ), 
    "disco3" ~: discDL "a a a" ~=? Node1 Loop la ]

pin = Place "I" "pI"
pout = Place "O" "pO"

emptyWFNet = WorkflowNet (fromList []) (fromList []) (fromList []) pin pout
tla = Transition "a" "t2"

vs = " == vs == "

plid x = map placeId (toList (places x))
trid x = map tranId (toList (transitions x))

wfid x = show (plid x ++ trid x)

cmp :: WorkflowNet -> WorkflowNet -> String
cmp x y | x == y = "Same"
        | show x == show y = "Id Diff: " ++ wfid x ++ vs ++ wfid y
        | places x == places y && transitions x == transitions y
                    && edges x /= edges y
                = "Edge Diff: " ++ show (edges x) ++ vs ++ show (edges y)
        | places x == places y && transitions x /= transitions y
                = "Transition Diff: " ++ show (transitions x) 
                        ++ vs ++ show (transitions y)
        | otherwise = "Diff: " ++ show x ++ vs ++ show y


pmidSeq = Place "" "p2"
tsa = Transition "a" "t4"
tsb = Transition "b" "t5"

tca = Transition "a" "t3"
tcb = Transition "b" "t4"

tcb1 = Transition "b" "t5"
tcc = Transition "c" "t6"

pmidLoop1 = Place "" "p2"
pmidLoop2 = Place "" "p3"
tlpa = Transition "a" "t6"
ttau1 = SilentTransition "t4"
ttau2 = SilentTransition "t5"


pmidConc1 = Place "" "p4"
pmidConc2 = Place "" "p5"
pmidConc3 = Place "" "p6"
pmidConc4 = Place "" "p7"
tcoa = Transition "a" "t9"
tcob = Transition "b" "t10"
ttau = SilentTransition  "t2"
ttauout = SilentTransition "t3"
translateConcExpected = 
    WorkflowNet (fromList[pin,pout,pmidConc1,pmidConc2,pmidConc3,pmidConc4])
                (fromList[tcoa,tcob,ttau,ttauout])
                (fromList[ToTransition pin ttau, 
                          ToPlace ttau pmidConc1, ToPlace ttau pmidConc2,
                          ToTransition pmidConc1 tcoa,
                          ToTransition pmidConc2 tcob,
                          ToPlace tcoa pmidConc3, ToPlace tcob pmidConc4,
                          ToTransition pmidConc3 ttauout,
                          ToTransition pmidConc4 ttauout,
                          ToPlace ttauout pout])
                pin pout 10

translateTests = [
    "translateLeaf" ~: translateWFNet la ~=? 
                    WorkflowNet (fromList [pin,pout]) (fromList [tla])
                                (fromList [ToTransition pin tla,
                                           ToPlace tla pout]) pin pout 2,
    "translateSeq" ~: translateWFNet (Node2 Seq la lb) ~=?
          WorkflowNet (fromList [pin,pmidSeq,pout]) (fromList [tsa,tsb])
                      (fromList [ToTransition pin tsa, ToPlace tsa pmidSeq,
                                 ToTransition pmidSeq tsb, ToPlace tsb pout] ) 
                                pin pout 5,
    "translateChoice1" ~: translateWFNet (Node2 Choice la lb) ~=?
        WorkflowNet (fromList [pin,pout]) (fromList [tca,tcb])
                  (fromList [ToTransition pin tca, ToPlace tca pout,
                             ToTransition pin tcb, ToPlace tcb pout] ) 
                   pin pout 4,
    "translateChoice2" ~: translateWFNet (Node2 Choice la 
                                                        (Node2 Choice lb lc)) 
        ~=?  WorkflowNet (fromList [pin,pout]) (fromList [tca,tcb1,tcc])
                  (fromList [ToTransition pin tca, ToPlace tca pout,
                             ToTransition pin tcb1, ToPlace tcb1 pout,
                             ToTransition pin tcc, ToPlace tcc pout] ) 
                   pin pout 6,
    "translateLoop" ~: 
        WorkflowNet (fromList [pin,pout,pmidLoop1]) 
                 (fromList [tlpa, ttau1, ttau2])
                 (fromList [ToTransition pin ttau1, ToPlace ttau1 pmidLoop1,
                            ToTransition pmidLoop1 tlpa, ToPlace tlpa pmidLoop1,
                            ToTransition pmidLoop1 ttau2, ToPlace ttau2 pout])
                   pin pout 6
                   ~=? translateWFNet (Node1 Loop la) ,
    "translateConc" ~: translateWFNet (Node2 Conc la lb) 
                            ~=? translateConcExpected

    ]

ruleTests = choiceIdTests ++ choiceFoldTests 
         ++ loopRollTests ++ loopNestTests
         ++ concIdTests ++ concFromChoiceTests ++ concSubsumeTests
         ++ silentSeqTests

commutTests = commutChoiceTests ++ commutConcTests

huTests = eqTests 
             ++ ruleTests ++ commutTests ++ longLoopTests 
             ++ transformTests
             ++ traceTests ++ discoverTests ++ translateTests


