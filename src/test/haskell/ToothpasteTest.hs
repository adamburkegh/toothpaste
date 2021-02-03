module ToothpasteTest where

import Toothpaste hiding (main)
import PetriNet
import EventLog
import Data.Set (Set,toList,fromList)

import System.Exit
import Test.HUnit

sptree x = traceModel $ parseDelimitedTrace x

choicet :: (Ord a) => [PPTree a] -> PPTree a
choicet [x] = x
choicet (x:xs) = Node2 Choice x cxs (weight x + weight cxs)
     where cxs = choice xs

la = Leaf "a" 1
lb = Leaf "b" 1
lc = Leaf "c" 1
ld = Leaf "d" 1
le = Leaf "e" 1
lf = Leaf "f" 1
lg = Leaf "g" 1
cab = Node2 Choice la lb 2
cab2 = Node2 Choice (Node2 Seq lb la 1) (Node2 Seq lb la 1) 2
cab3 = Node2 Choice (Node2 Seq lb la 1) (Node2 Seq lb lb 1) 2
cabc1 = choiceP (seqP lb la) (seqP lb lc)
saa = Node2 Seq la la 1
saaa = seqP la (Node2 Seq la la 1)
saaab =  sptree "a a a b"


sabcd = sptree "a b c d"
sefgd = sptree "e f g d"

lab = sptree "a b a b"
sab = Node2 Seq la lb 1
sba = Node2 Seq lb la 1

ccab1 = Node2 Conc la lb 2

la2 = Leaf "a" 2
la3 = Leaf "a" 3
lb2 = Leaf "b" 2
lb3 = Leaf "b" 3
lc2 = Leaf "c" 2
ld2 = Leaf "d" 2
loopa1 = Node1 FLoop la 1 1

-- tests

eqTests = ["eqLeaf1" ~: Leaf "a" 1 ~=? Leaf "a" 1,
           "eqLeaf2" ~: Leaf "a" 2 /= Leaf "b" 2 @? "neq",
           "eqLeaf3" ~: Leaf "a" 2 /= cab @? "neq",
           "eqLeaf4"     ~: Leaf "a" 1 /= Leaf "a" 2 @? "neq",
           "neqSilent1"  ~: silent 1 /= silent 2 @? "neq",
           "eqSilent"    ~: silent 0 ~=? emptyTree,
           "eqNode1" ~: cab ~=? cab,
           "eqNode2" ~: cab /= cab2 @? "neq"]

choiceSimTests = ["choiceSim1" ~: la2 ~=? choiceSim(  Node2 Choice la la 2),
                 "choiceSim2" ~: choiceSim cab  ~=? cab,
                 "choiceSim3" ~: choiceSim cab2 ~=? Node2 Seq lb2 la2 2,
                 "choiceSim4" ~: choiceSim cab3 ~=? cab3,
                 "choiceSimLoop" ~: Node1 PLoop la2 6 2
                    ~=? choiceSim (Node2 Choice (Node1 PLoop la 7 1) 
                                                (Node1 PLoop la 5 1) 2) ]


-- consider a commute normalizer for testing?
commutChoiceSim = trule (commutTRule Choice choiceSim "choiceSim")

commutChoiceTests = [ 
    "cchoiceSim1" ~: 
                        commutChoiceSim (choice[la,lb,lc,ld,le,la]) ~=?
                            Node2 Choice 
                                (Node2 Choice 
                                    (Node2 Choice 
                                             (Node2 Choice la2 le 3) 
                                             ld 4) 
                                    lc 5) 
                                lb 6,
    "cchoiceSim2" ~: 
          Node2 Choice (Node2 Choice la2
                                     (Node2 Choice lc
                                                   (Node2 Seq la lc 1) 
                                                   2) 
                                     4)
                       lb 5
          ~=? commutChoiceSim (choice [la,lb,lc,la, Node2 Seq la lc 1]),
    "cchoiceSim3" ~: 
                        Node2 Choice (Node2 Choice la2 lc 3) lb 4 ~=? 
                            commutChoiceSim (choicet [la,lb,lc,la]) ]

commutConcChoice = trule (commutTRule Choice concFromChoice "concFromChoice")

cc2x = Node2 Choice (Node2 Choice sba 
                                  (Node2 Choice (Node1 PLoop lb 3 1) 
                                                sab 2) 
                                  3) 
                    (Node1 PLoop la 2 1) 
                    4

commutConcTests = [ 
    "cconcChoice1" ~: Node2 Choice 
                            (Node2 Choice 
                                (Node2 Choice 
                                    (Node2 Choice ccab1 lb 3) 
                                    la 4) 
                                la 5)
                            la 6 ~=? 
    commutConcChoice (choicet [ sab, la, la, la, sba, lb]) ,
    "cconcChoice2" ~: Node2 Choice (Node2 Choice ccab1 
                                                 (Node1 PLoop lb 3 1) 3) 
                                   (Node1 PLoop la 2 1) 4 
                      ~=?  commutConcChoice cc2x ,
    "cconcChoice3" ~: Node2 Choice ccab1 la 3 
                      ~=? commutConcChoice( Node2 Choice sba 
                                                (Node2 Choice la sab 2) 3) ]


choiceFoldTests = [
    "choiceFold1" ~: choiceFold la ~=? la,
    "choiceFold2" ~: choiceFold cab  ~=? cab,
    "choiceFold3" ~: choiceFold cab2 ~=? 
                                Node2 Seq lb2 (Node2 Choice la la 2) 2,
    "choiceFoldId1" ~: 
           choiceSim (choiceFold cab2) /= choiceFold (choiceSim cab2) 
                                @? "neq",
    "choiceFoldLongSuffix" ~:  
           Node2 Seq (Node2 Choice (Node2 Seq la 
                                              (Node2 Seq lb lc 1) 1) 
                                   (Node2 Seq le  
                                              (Node2 Seq lf lg 1) 1) 
                                   2)
                      ld2 2
                            ~=? choiceFold (Node2 Choice sabcd sefgd 2) ]

choiceSkipTests = [
    "choiceSkipNone" ~: la ~=? choiceSkip la,
    "choiceSkip1"    ~: Node2 Seq la2 (Node2 Choice lb (Silent 1) 2) 2 
                    ~=? choiceSkip ( Node2 Choice la 
                                                  (Node2 Seq la lb 1)
                                                  2 )

    ]

clbca = Node2 Choice (Node2 Seq la lc 1) 
                     (Node2 Seq (Node1 PLoop la 1 1) 
                                lb 1)  2

loopChoiceFoldTests = [
    "loopChoiceFold1" ~: loopChoiceFold la ~=? la,
    "loopChoiceFold2" ~: loopChoiceFold cab  ~=? cab,
    "loopChoiceFold3" ~: Node2 Seq (Node1 PLoop  la 1 2)
                                   (Node2 Choice lb lc 2) 
                                   2
            ~=? loopChoiceFold clbca ,
    "loopChoiceFoldId1" ~: 
           choiceSim (choiceFold cab2) /= loopChoiceFold (choiceSim cab2) 
                                @? "neq",
    "loopChoiceFoldLongSuffix" ~:  
           Node2 Seq (Node2 Choice (Node2 Seq (Node2 Seq lb lc 1) 
                                              la 1) 
                                   (Node2 Seq (Node2 Seq lf lg 1) 
                                              le 1) 
                                   2)
                      (Node1 PLoop ld 1 1) 2
                            /= loopChoiceFold (Node2 Choice sabcd sefgd 2)
                            @? "impl gap" ]

choiceRollTests = [
    "choiceRoll1" ~: choiceRoll la ~=? la,
    "choiceRoll2" ~: Node1 PLoop la 4 4 ~=? 
                choiceRoll (Node2 Choice la 
                                         (Node1 PLoop la 5 3) 4) ]


fixedLoopRollTests = [
    "floopRoll1" ~: fixedLoopRoll la ~=? la,
    "floopRoll2" ~: fixedLoopRoll saa  ~=? Node1 FLoop la 2 1,
    "floopRollMid1" ~: fixedLoopRoll saaa ~=? 
                            Node2 Seq (Node1 FLoop la 2 1) la 1]


probLoopRollTests = [
    "ploopRoll1" ~: probLoopRoll la ~=? la,
    "ploopRoll2" ~: probLoopRoll saa  ~=? Node1 PLoop la 2 1,
    "ploopRollMid1" ~: probLoopRoll saaa ~=? 
                            Node2 Seq (Node1 PLoop la 2 1) la 1]

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
    "loopGeoSim" ~: Node1 PLoop lb2 3 2
        ~=? loopGeo (Node2 Choice (Node1 FLoop lb 2 1)
                                  (Node1 FLoop lb 4 1) 2),
    "loopGeoId" ~: Node1 PLoop lb2 2 2
        ~=? loopGeo (Node2 Choice (Node1 FLoop lb 2 1)
                                  (Node1 FLoop lb 2 1) 2),
    "loopGeoNe" ~: loopGeo (Node2 Choice (Node1 FLoop lb 2 1)
                                  (Node1 FLoop la 4 1) 2)
        ~=? loopGeo (Node2 Choice (Node1 FLoop lb 2 1)
                                  (Node1 FLoop la 4 1) 2)
    ]

concSimTests = [ 
    "concSim1" ~: concSim la ~=? la,
    "concSim2" ~: concSim(Node2 Conc la la 2) ~=? Node1 FLoop la2 2 2,
    "concSim3" ~: concSim(Node2 Conc la lb 2) ~=? Node2 Conc la lb 2]


concFromChoiceTests = [ 
    "concFromChoice1" ~: concFromChoice ccab1 ~=? ccab1,
    "concFromChoice2" ~: concFromChoice la  ~=? la,
    "concFromChoice3" ~: concFromChoice( Node2 Choice sab sba 2) 
                                ~=? Node2 Conc la lb 2 ]


concSubsumeTests = [
    "concSubsume1" ~: concSubsume la ~=? la,
    "concSubsume2" ~: Node2 Conc la2 lb 3 
                ~=? concSubsume (Node2 Choice (Node2 Seq la lb 1) 
                                              (Node2 Conc la lb 2) 3) ]

silentSeqTests = [
    "silentSeq1Test" ~: silentSeq ccab1 ~=? ccab1,
    "silentSeq2Test" ~: silentSeq (Node2 Seq la (Silent 1) 1) ~=? la ]

nlr = longSeq (Node2 Seq la 
                    (Node2 Seq la 
                          (Node2 Seq lb 
                                    (Node2 Seq la 
                                            (Node2 Seq (Node1 FLoop 
                                                                (Node2 Seq lb 
                                                                           la 
                                                                           1)
                                                                 2 1) 
                                                        lc 1) 1) 1) 1) 1) 

longLoopTests = [
    "prefix1" ~: prefix la saaa @? "prefix",
    "prefix2" ~: not (prefix lb saaa) @? "not prefix",
    "rmpref1" ~: rmpref la lb ~=? lb,
    "rmpref2" ~: rmpref la saaa ~=? Silent 1,
    "rmpref3" ~: rmpref la saaab ~=? lb,
    "longSeq1" ~: longSeq lb ~=? lb,
    "longSeq2" ~: longSeq lab ~=? 
            Node1 FLoop (Node2 Seq la lb 1) 2 1,
    "longSeq3" ~: longSeq nlr ~=?  nlr ]


transformTests = [
    "transformBreadth1" ~: la ~=? transform la,
    "transformBreadth2" ~: transform cab2 ~=? Node2 Seq lb2 la2 2,
    "transformBreadth3" ~: transform cab3 ~=? Node2 Choice
                                            (Node2 Seq lb la 1)
                                            (Node1 PLoop lb 2 1) 2,
    "transform4" ~: Node2 Seq lb2
                              (Node2 Choice la lc 2) 2
                    ~=? transform cabc1            ]


traceTests = [
    "tracePTree1" ~: tracePPTree 1 ["a"] ~=? la,
    "tracePTree2" ~: scale (Node2 Seq la lb 1) 9 ~=? tracePPTree 9 ["a","b"] ,
    "tracePTree3" ~: tracePPTree 1 ["a","b","c"] ~=?
                            Node2 Seq la (Node2 Seq lb lc 1) 1, 
    "traceModel1" ~: traceModelR 1 [["a"]] ~=? la,
    "traceModel2" ~: traceModelR 1 [["a","b","c"]] ~=?
                            Node2 Seq la (Node2 Seq lb lc 1) 1,
    "traceModel3" ~: Node2 Choice
                                (Node2 Seq la
                                          (Node2 Seq lb lc 1) 1) 
                                (Node2 Seq la lb 1) 
                                2 ~=? 
                     traceModelR 1 [["a","b","c"],["a","b"]],
    "traceModelCount" ~: traceModelR 1 [["a"],["a"],["c"]] ~=?
                            Node2 Choice la2 lc 3 ,
    "traceModelLong" ~: Node2 Choice
                                (Node2 Seq la lb 1) 
                                (Node2 Choice lb lc 2)
                                3
                        ~=? traceModelR 1 [["a","b"],["b"],["c"]] 
        ]

discDL = discover parseDelimitedTrace

discoverTests = [
    "disco1" ~: la ~=? discDL "a", 
    "disco2" ~: Node2 Seq la2 
                          (Node2 Seq lb2
                                     (Node2 Choice lc 
                                                   (Silent 1) 2) 
                                                    2) 2
        ~=? discDL "a b c\na b",
    "disco3" ~: Node1 PLoop la 3 1 ~=? discDL "a a a" ]


pin = Place "I" "pI"
pout = Place "O" "pO"

emptyWFNet = WorkflowNet (fromList []) (fromList []) (fromList []) pin pout
tla = WTransition "a" "t2" 1

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
tsa = WTransition "a" "t4" 1
tsb = WTransition "b" "t5" 1

tca = WTransition "a" "t3" 1
tcb = WTransition "b" "t4" 1

tcb1 = WTransition "b" "t5" 1
tcc = WTransition "c" "t6" 1

pmidLoop1 = Place "" "p2" 
pmidLoop2 = Place "" "p3" 
tlpa = WTransition "a" "t6" 5
ttauin1 = WTransition "tauin" "t4" 1
ttauout2 = WTransition "tauout" "t5" 1


pmidConc1 = Place "" "p4"
pmidConc2 = Place "" "p5"
pmidConc3 = Place "" "p6"
pmidConc4 = Place "" "p7"
tcoa = WTransition "a" "t9" 1
tcob = WTransition "b" "t10" 1
ttau = WTransition "tau" "t2" 1
ttau2= WTransition "tau" "t2" 2
ttauout = WTransition "tau" "t3" 1
translateConcExpected = 
    WeightedNet (fromList[pin,pout,pmidConc1,pmidConc2,pmidConc3,pmidConc4])
                (fromList[tcoa,tcob,ttau2,ttauout])
                (fromList[WToTransition pin ttau2, 
                          WToPlace ttau2 pmidConc1, WToPlace ttau2 pmidConc2,
                          WToTransition pmidConc1 tcoa,
                          WToTransition pmidConc2 tcob,
                          WToPlace tcoa pmidConc3, WToPlace tcob pmidConc4,
                          WToTransition pmidConc3 ttauout,
                          WToTransition pmidConc4 ttauout,
                          WToPlace ttauout pout])
                pin pout 10

translateTests = [
    "translateLeaf" ~: translate la ~=? 
                    WeightedNet (fromList [pin,pout]) (fromList [tla])
                                (fromList [WToTransition pin tla,
                                           WToPlace tla pout]) pin pout 2,
    "translateSeq" ~: translate (Node2 Seq la lb 1) ~=?
          WeightedNet (fromList [pin,pmidSeq,pout]) (fromList [tsa,tsb])
                      (fromList [WToTransition pin tsa, WToPlace tsa pmidSeq,
                                 WToTransition pmidSeq tsb, 
                                 WToPlace tsb pout] ) 
                                pin pout 5,
    "translateChoice1" ~: translate (Node2 Choice la lb 2) ~=?
        WeightedNet (fromList [pin,pout]) (fromList [tca,tcb])
                  (fromList [WToTransition pin tca, WToPlace tca pout,
                             WToTransition pin tcb, WToPlace tcb pout] ) 
                   pin pout 4,
    "translateChoice2" ~: 
        translate (Node2 Choice la 
            (Node2 Choice lb lc 2) 3) 
        ~=?  WeightedNet (fromList [pin,pout]) (fromList [tca,tcb1,tcc])
                  (fromList [WToTransition pin tca, WToPlace tca pout,
                             WToTransition pin tcb1, WToPlace tcb1 pout,
                             WToTransition pin tcc, WToPlace tcc pout] ) 
                   pin pout 6,
    "translateLoop" ~: 
        WeightedNet (fromList [pin,pout,pmidLoop1]) 
                 (fromList [tlpa, ttauin1, ttauout2])
                 (fromList [WToTransition pin ttauin1, 
                            WToPlace ttauin1 pmidLoop1,
                            WToTransition pmidLoop1 tlpa, 
                            WToPlace tlpa pmidLoop1,
                            WToTransition pmidLoop1 ttauout2,
                            WToPlace ttauout2 pout])
                   pin pout 6
                   ~=? translate (Node1 PLoop la 5 1) ,
    "translateConc" ~: translateConcExpected 
                   ~=?  translate (Node2 Conc la lb 2) 

    ]

csimRule :: (Eq a, Ord a) => TRule a
csimRule = TRule{rulename="choiceSim", trule=choiceSim}

trl :: (Eq a, Ord a) => [TRule a]
trl = [csimRule]

transformInOrderTests = [
    "troLeaf" ~: la ~=? transformPT la csimRule,
    "troChoiceNoSim" ~: (Node2 Choice la lb 2)  
            ~=? transformPT (Node2 Choice la lb 2) csimRule,
    "troChoiceSim" ~: la2 ~=? transformPT (Node2 Choice la la 2) csimRule,
    "troChoiceAtSeqDepth" ~:  
            Node2 Seq (Node2 Seq (Node2 Choice la lb 2) la2 2) lc 2
        ~=? transformPT (Node2 Seq (Node2 Seq (Node2 Choice la lb 2) 
                                              (Node2 Choice la la 2) 2 ) 
                                   lc 2) csimRule,
    "transformRO1" ~: la ~=? transformRuleOrdered la,
    "transformRO2" ~: transformRuleOrdered cab2 ~=? Node2 Seq lb2 la2 2,
    "transformRO3" ~: Node2 Seq lb2 (Node2 Choice la lb 2) 2 
                        ~=? transformRuleOrdered cab3  ,
    "transformRO4" ~: Node2 Seq lb2
                              (Node2 Choice la lc 2) 2
                    ~=? transformRuleOrdered cabc1            ]



simTests   = ["simLeafId"  ~: la =~= la @? "sim",
              "simLeafId"  ~: la =~= la3 @? "sim",
              "simChoice"  ~: Node2 Choice la lb 2 =~= Node2 Choice la3 lb2 9
                                                            @? "sim"]

mergeTests = ["mergeLeaf" ~: la2 ~=? merge la la,
              "mergeLoop" ~: Node1 FLoop la2 1 2 ~=? merge loopa1 loopa1,
              "mergeChoice" ~: Node2 Choice la2 lb2 4
                            ~=? merge (Node2 Choice la lb 2) 
                                      (Node2 Choice la lb 2) ]


scaleTests = [
    "scaleLeaf" ~: la3 ~=? scale la 3,
    "scaleLoop" ~: Node1 FLoop la3 5 3 ~=? scale (Node1 FLoop la 5 1) 3 ]

loopMeanTests = ["loopMean1" ~: Node1 PLoop la 2.5 4 ~=? 
                    loopMean la (Node1 PLoop la 3 3) ]

validateTests = [
    "validSeq"   ~: validate (Node2 Seq la lb 1) @? "val",
    "invalidSeq" ~: not ( validate (Node2 Seq la lb2 4) ) @? "inval"  ]


loopRollTests = fixedLoopRollTests  ++ probLoopRollTests

ruleTests = choiceSimTests ++ choiceFoldTests ++ choiceSkipTests
          ++ loopChoiceFoldTests ++ choiceRollTests
          ++ loopRollTests ++ loopNestTests ++ loopGeoTests
          ++ concSimTests ++ concFromChoiceTests ++ concSubsumeTests
          ++ silentSeqTests

commutTests = commutChoiceTests ++ commutConcTests 

helperTests = mergeTests ++ scaleTests ++ loopMeanTests

huTests =   eqTests ++ helperTests
           ++ ruleTests ++ commutTests ++ longLoopTests
           ++ transformTests ++ transformInOrderTests
           ++ traceTests ++ discoverTests  ++ translateTests
           ++ validateTests


