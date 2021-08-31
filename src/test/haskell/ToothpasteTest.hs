module ToothpasteTest where

import PetriNet
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
cab2 = NodeN Choice [(NodeN Seq [lb,la] 1),(NodeN Seq [lb,la] 1)] 2
cab3 = NodeN Choice [(NodeN Seq [lb,la] 1),(NodeN Seq [lb,lb] 1)] 2
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
                    ~=? choiceSim (NodeN Choice [(Node1 PLoop la 7 1),
                                                 (Node1 PLoop la 5 1)] 2) ]

concSimTests = [
    "concSim1" ~: la ~=? concSim la,
    "concSim2" ~: NodeN Conc [Node1 FLoop la2 2 2] 2 
                        ~=? concSim(NodeN Conc [la,la] 2) ,
    "concSim3" ~: NodeN Conc [la,lb] 2 ~=? concSim(NodeN Conc [la,lb] 2)  
                ]

flattenTests = [
    "leaf"          ~: la ~=? flatten la,
    "choiceNoop"    ~: NodeN Choice [la,NodeN Seq [lb,lc] 2] 3
                ~=? flatten (NodeN Choice [la,NodeN Seq [lb,lc] 2] 3),
    "choiceFlatten" ~: NodeN Choice [la,lb,lc] 3 
                ~=? flatten (NodeN Choice [la, NodeN Choice [lb,lc] 2] 3 ) ,
    "seqFlatten" ~: NodeN Choice [la,lb,lc] 3 
                ~=? flatten (NodeN Choice [la, NodeN Choice [lb,lc] 2] 3 ) 
    ]

--

transformInOrderSimpleTests = [
    "transformRONoop" ~: la ~=? ssRuleOrdered la,
    "transformROsilent" ~: NodeN Seq [lb2,la2] 2 
                        ~=? ssRuleOrdered (NodeN Seq [lb2,(Silent 2),la2] 2),
    "transformROSilentSingle" ~: NodeN Seq [lb2,la2] 2
            ~=? ssRuleOrdered (NodeN Seq [lb2,(NodeN Seq [la2,Silent 2] 2)] 2)]


-- Petri net conversion
pin = Place "I" "pI"
pout = Place "O" "pO"

tla = WTransition "a" "t2" 1

pmidSeq  = Place "" "p2"
pmidSeq2 = Place "" "p4"
tsa = WTransition "a" "t3" 1
tsb4 = WTransition "b" "t4" 1
tsb5 = WTransition "b" "t5" 1
tsc = WTransition "c" "t6" 1

tca = WTransition "a" "t3" 1
tcb = WTransition "b" "t4" 1

tcb1 = WTransition "b" "t5" 1
tcc = WTransition "c" "t6" 1

pmidLoop1 = Place "" "p2"
pmidLoop2 = Place "" "p3"
tlpa = WTransition "a" "t6" 5
ttauin1 = WTransition "tauin" "t4" 1
ttauout2 = WTransition "tauout" "t5" 1


pmidConcain = Place "" "p4"
pmidConcaout = Place "" "p5"
pmidConcbin = Place "" "p7"
pmidConcbout = Place "" "p8"
tcoa = WTransition "a" "t6" 1
tcob = WTransition "b" "t9" 1
ttauin2= WTransition "tau" "t2" 2
ttauout3 = WTransition "tau" "t3" 1
translateConcExpected =
    WeightedNet (fromList[pin,pout,pmidConcain,pmidConcaout,
                          pmidConcbin,pmidConcbout])
                (fromList[tcoa,tcob,ttauin2,ttauout3])
                (fromList[WToTransition pin ttauin2,
                          WToPlace ttauin2 pmidConcain, 
                          WToPlace ttauin2 pmidConcbin,
                          WToTransition pmidConcain tcoa,
                          WToTransition pmidConcbin tcob,
                          WToPlace tcoa pmidConcaout, 
                          WToPlace tcob pmidConcbout,
                          WToTransition pmidConcaout ttauout3,
                          WToTransition pmidConcbout ttauout3,
                          WToPlace ttauout3 pout])
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


translateTests = [
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
                                (NodeN Choice [lb,lc] 2)] 3)
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
                   ~=?  translate (NodeN Conc [la,lb] 2)

    ]



--

ruleTests   = silentSeqTests ++ singleNodeOpTests 
           ++ choiceSimTests ++ concSimTests
           ++ flattenTests

transformTests = transformInOrderSimpleTests

huTests     = eqTests
            ++ transformTests
            ++ translateTests
            ++ ruleTests


