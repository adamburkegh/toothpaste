--
-- Note in these tests that transition and place ids are included in 
-- comparisons, but not in the default string representations for weighted nets.
-- So tests can fail showing the compared nets as identical, because the 
-- underlying ids are different. These tests are based on the exact order
-- of the generated ids.
-- 

module TPMineTest where

import PetriNet
import ProbProcessTree
import Toothpaste hiding (main)
import TPMine

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
cabc1 = NodeN Choice [la,lb,lc] 1
saa = NodeN Seq [la,la] 1



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
ld2 = Leaf "d" 2


-- Tests


-- Petri net conversion
pin = Place "I" "pI"
pout = Place "O" "pO"

tla = wtransition "a" "t2" 1

pmidSeq  = Place "" "p2"
pmidSeq2 = Place "" "p4"
tsa = wtransition "a" "t3" 1
tsb4 = wtransition "b" "t4" 1
tsb5 = wtransition "b" "t5" 1
tsc = wtransition "c" "t6" 1

tca = wtransition "a" "t3" 1
tcb = wtransition "b" "t4" 1

tcb1 = wtransition "b" "t5" 1
tcc = wtransition "c" "t6" 1

pmidLoop1 = Place "" "p2"
pmidLoop2 = Place "" "p3"


pmidConcain = Place "" "p4"
pmidConcaout = Place "" "p5"
pmidConcbin = Place "" "p7"
pmidConcbout = Place "" "p8"
tcoa = wtransition "a" "t6" 1
tcob = wtransition "b" "t9" 1
ttau2 = silentTransition "tau" "t2" 2
ttau3 = silentTransition "tau" "t3" 2
translateConcExpected =
    WeightedNet (fromList[pin,pout,pmidConcain,pmidConcaout,
                          pmidConcbin,pmidConcbout])
                (fromList[tcoa,tcob,ttau2,ttau3])
                (fromList[WToTransition pin ttau2,
                          WToPlace ttau2 pmidConcain, 
                          WToPlace ttau2 pmidConcbin,
                          WToTransition pmidConcain tcoa,
                          WToTransition pmidConcbin tcob,
                          WToPlace tcoa pmidConcaout, 
                          WToPlace tcob pmidConcbout,
                          WToTransition pmidConcaout ttau3,
                          WToTransition pmidConcbout ttau3,
                          WToPlace ttau3 pout])
                pin pout 9

vs = " \n== vs == \n"

plid x = map placeId (toList (wnplaces x))
trid x = map wtranId (toList (wntransitions x))

vshowEdge :: (Show a) => WEdge a -> String
vshowEdge (WToPlace t p) = show (wtranId t) ++ "->" ++ show (placeId p)
vshowEdge (WToTransition p t) = show (placeId p) ++ "->" ++ show (wtranId t)

eid x  = map vshowEdge (toList (wnedges x))

wnid x = show( sort (plid x ++ trid x) ) 

cmpWN :: WeightedNet -> WeightedNet -> String
cmpWN x y | x == y = "Same"
          | show x == show y = "Id Diff:\n" ++ wnid x ++ vs ++ wnid y
          | wnplaces x == wnplaces y && wntransitions x == wntransitions y
                    && wnedges x /= wnedges y
                = "Edge Diff:\n" ++ show (wnedges x) ++ vs ++ show (wnedges y)
          | wnplaces x == wnplaces y && wntransitions x /= wntransitions y
                = "Transition Diff:\n" ++ show (wntransitions x)
                        ++ vs ++ show (wntransitions y)
          | otherwise = "Diff:\n" ++ show x ++ vs ++ show y

diffWN :: WeightedNet -> WeightedNet -> IO ()
diffWN x y = putStrLn (cmpWN x y)


translateRest = [
    "translateLeaf" ~: translate la ~=?
                    WeightedNet (fromList [pin,pout]) (fromList [tla])
                                (fromList [WToTransition pin tla,
                                           WToPlace tla pout]) pin pout 2,
    "translateSeq" ~: 
          WeightedNet (fromList [pin,pmidSeq,pout]) (fromList [tsa,tsb4])
                      (fromList [WToTransition pin tsa, WToPlace tsa pmidSeq,
                                 WToTransition pmidSeq tsb4,
                                 WToPlace tsb4 pout] )
                                 pin pout 4
                       ~=? translate (NodeN Seq [la,lb] 1)  ,
    "translateSeq3" ~: 
          WeightedNet (fromList[pin,pmidSeq,pmidSeq2,pout])
                      (fromList[tsa,tsb5,tsc])
                      (fromList [WToTransition pin tsa, 
                                 WToPlace tsa pmidSeq,
                                 WToTransition pmidSeq tsb5,
                                 WToPlace tsb5 pmidSeq2,
                                 WToTransition pmidSeq2 tsc,
                                 WToPlace tsc pout] )
                                 pin pout 6
                    ~=? translate (NodeN Seq [la,lb,lc] 1) ,
    "translateChoice1" ~: translate (NodeN Choice [la,lb] 2) ~=?
        WeightedNet (fromList [pin,pout]) (fromList [tca,tcb])
                  (fromList [WToTransition pin tca, WToPlace tca pout,
                             WToTransition pin tcb, WToPlace tcb pout] )
                   pin pout 4,
    "translateChoice2" ~:
        translate (NodeN Choice [la,
                                NodeN Choice [lb,lc] 2] 3)
        ~=?  WeightedNet (fromList [pin,pout]) (fromList [tca,tcb1,tcc])
                  (fromList [WToTransition pin tca, WToPlace tca pout,
                             WToTransition pin tcb1, WToPlace tcb1 pout,
                             WToTransition pin tcc, WToPlace tcc pout] )
                   pin pout 6,
    "translateConc" ~: translateConcExpected
                   ~=?  translate (NodeN Conc [la,lb] 2)
    ]


la5  = Leaf "a" 5
la8  = Leaf "a" 8
la10 = Leaf "a" 10
lb5  = Leaf "b" 5
lb8  = Leaf "b" 8


tlpa32_5 = wtransition "a" "t7" (32/5)
tlpa4_id6    = wtransition "a" "t6" 4
tlpa4_id7    = wtransition "a" "t7" 4
tlpa8    = wtransition "a" "t6" 8

tlpb4   = wtransition "b" "t8" 4
tlpb8_5 = wtransition "b" "t8" (8/5)


ttauin2  = silentTransition "tauin" "t4" 2
ttauin5  = silentTransition "tauin" "t4" 5
ttauin10 = silentTransition "tauin" "t4" 10

ttauout1 = silentTransition "tauout" "t5" 1
ttauout2 = silentTransition "tauout" "t5" 2


translatePLoops = [
    "translateLoopLeaf" ~:
        WeightedNet (fromList [pin,pout,pmidLoop1])
                 (fromList [tlpa4_id6, ttauin5, ttauout1])
                 (fromList [WToTransition pin ttauin5,
                            WToPlace ttauin5 pmidLoop1,
                            WToTransition pmidLoop1 tlpa4_id6,
                            WToPlace tlpa4_id6 pmidLoop1,
                            WToTransition pmidLoop1 ttauout1,
                            WToPlace ttauout1 pout])
                   pin pout 6
                   ~=? translate (Node1 PLoop la5 5 5) ,
    "translateLoopLeafNonUnityOutput" ~:
        WeightedNet (fromList [pin,pout,pmidLoop1])
                 (fromList [tlpa8, ttauin10, ttauout2])
                 (fromList [WToTransition pin ttauin10,
                            WToPlace ttauin10 pmidLoop1,
                            WToTransition pmidLoop1 tlpa8,
                            WToPlace tlpa8 pmidLoop1,
                            WToTransition pmidLoop1 ttauout2,
                            WToPlace ttauout2 pout])
                   pin pout 6
                   ~=? translate (Node1 PLoop la10 5 10) ,
    "translateLoopChoice" ~:
       WeightedNet (fromList [pin,pout,pmidLoop1])
                   (fromList [tlpa4_id7, tlpb4, ttauin10, ttauout2])
                   (fromList [WToTransition pin ttauin10,
                              WToPlace ttauin10 pmidLoop1,
                              WToTransition pmidLoop1 tlpa4_id7,
                              WToPlace tlpa4_id7 pmidLoop1,
                              WToTransition pmidLoop1 tlpb4,
                              WToPlace tlpb4 pmidLoop1,
                              WToTransition pmidLoop1 ttauout2,
                              WToPlace ttauout2 pout])
                  pin pout 8
                  ~=? translate (Node1 PLoop (NodeN Choice [la5,lb5] 10) 5 10),
    "translateLoopChoiceUneven" ~:
       WeightedNet (fromList [pin,pout,pmidLoop1])
                   (fromList [tlpa32_5, tlpb8_5, ttauin10, ttauout2])
                   (fromList [WToTransition pin ttauin10,
                              WToPlace ttauin10 pmidLoop1,
                              WToTransition pmidLoop1 tlpa32_5,
                              WToPlace tlpa32_5 pmidLoop1,
                              WToTransition pmidLoop1 tlpb8_5,
                              WToPlace tlpb8_5 pmidLoop1,
                              WToTransition pmidLoop1 ttauout2,
                              WToPlace ttauout2 pout])
                  pin pout 8
                  ~=? translate (Node1 PLoop (NodeN Choice [la8,lb2] 10) 5 10)
                ]


tla5_1 = wtransition "a" "t3" 5
tla5_2 = wtransition "a" "t4" 5
pfl1 = Place "" "p2"

translateFLoops = [
    "translateLoopLeaf" ~:
        WeightedNet (fromList [pin,pout,pfl1] )
                 (fromList [tla5_1,tla5_2])
                 (fromList [WToPlace tla5_1 pfl1,
                            WToPlace tla5_2 pout,
                            WToTransition pfl1 tla5_2,
                            WToTransition pin  tla5_1 ])
                   pin pout 4
                   ~=? translate (Node1 FLoop la5 2 5),
    "translateLoopLeaf3" ~:
                   translate (NodeN Seq [la5,la5,la5] 5)
                   ~=? translate (Node1 FLoop la5 3 5)
                ]

translateTests = translatePLoops ++ translateFLoops ++ translateRest 

validationTests = [
    "validNet1" ~: valOk 
            ~=? validateWeightedNet 
                    ( WeightedNet (fromList [pin,pout]) 
                                  (fromList [tca,tcb1,tcc])
                                  (fromList [WToTransition pin tca, 
                                             WToPlace tca pout,
                                             WToTransition pin tcb1, 
                                             WToPlace tcb1 pout,
                                             WToTransition pin tcc, 
                                             WToPlace tcc pout] )
                                  pin pout 6) ,
    "invalidNet1" ~: False
            ~=? valResult 
                    (validateWeightedNet 
                    ( WeightedNet (fromList [pin,pout]) 
                                  (fromList [tca,tcb1,tcc])
                                  (fromList [WToTransition pin tca, 
                                             WToPlace tca pout,
                                             WToTransition pin tcb1, 
                                             WToPlace tcb1 pout,
                                             -- WToTransition pin tcc, 
                                             WToPlace tcc pout] )
                                  pin pout 6)  )
    ]


traceConsolidateTests = [
    "empty" ~: ([]::[PPTree Int]) ~=? traceConsolidate [],
    "singleton" ~: [la] ~=? traceConsolidate [["a"]],
    "twoDiff" ~:  [la,lb] ~=? traceConsolidate [["a"],["b"]],
    "twoDiffSeq" ~:  [la,seqP [lb,lc] 1] ~=? traceConsolidate [["a"],["b","c"]],
    "twoSame" ~: [la2] ~=?  traceConsolidate [["a"],["a"]],
    "threeSame" ~: [la3] ~=?  traceConsolidate [["a"],["a"],["a"]],
    "fourSame" ~: [la4] ~=?  traceConsolidate [["a"],["a"],["a"],["a"]]
    ]

--

huTests     =  translateTests ++ validationTests ++ traceConsolidateTests


