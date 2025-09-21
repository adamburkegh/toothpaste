{-# LANGUAGE ImplicitParams #-}

module Toothpaste.WeightedAutomataTest where

import Toothpaste.WeightedAutomata

import Data.Set (Set,empty,fromList)
import qualified Data.Bimap as BM


import Test.HUnit
import Test.HUnit.Approx


testeps = 0.0001

wa1 = WSFA 1 1 (BM.insert 1 [] BM.empty)


etrain      = EdgeDetails 1 2 (Just "train") 1

watrain1    = wsfaFromList 1 2 [ 1 --< ("train",1) >-- 2 ]

watrainbusseq = wsfaFromList 1 3
                [ 1 --< ("train", 1) >-- 2 ,
                  2 --< ("bus",   1) >-- 3 ]

watrainbuschoice = wsfaFromList 1 2
                [ 1 --< ("train", 2) >-- 2 ,
                  1 --< ("bus",   1) >-- 2 ]

watrainbusbikechoice = wsfaFromList 1 2
                [ 1 --< ("train", 2) >-- 2,
                  1 --< ("bus",   1) >-- 2,
                  1 --< ("bike",  1) >-- 2]

waempty1    = wsfaFromList 1 2 [ 1 --<* (1) >-- 2 ]

wachoiceseq = wsfaFromList 1 3
                [ 1 --< ("a",  2) >-- 2,
                  1 --< ("b",  1) >-- 2,
                  2 --< ("c",  1) >-- 3]

wa2choice =  wsfaFromList 1 3
                [ 1 --< ("a",  2) >-- 2,
                  1 --< ("b",  1) >-- 2,
                  2 --< ("c",  1) >-- 3,
                  2 --< ("d",  1) >-- 3 ]

wasilentchoice = wsfaFromList 1 2
                [ 1 --< ("a", 2) >-- 2 ,
                  1 --<*(     1) >-- 2 ]

waloop1     = wsfaFromList 1 2
                [ 1 --< ("a", 1) >-- 1,
                  1 --< ("b", 2) >-- 2 ]

wasilloop1  = wsfaFromList 1 2
                [ 1 --<* (    1) >-- 1,
                  1 --< ("b", 3) >-- 2 ]

probBasicTests = 
    [ "noEdge"      ~: 0 ~=? wprob ["a"] wa1 ,
      "noMatch"     ~: 0 ~=? wprob ["none"] watrain1,
      "oneMatch"    ~: 1 ~=? wprob ["train"] watrain1,
      "twoMatch"    ~: 1 ~=? wprob ["train","bus"] watrainbusseq,
      "twoNoMatch"  ~: 0 ~=? wprob ["train"] watrainbusseq,
      "oneSilent"   ~: 1 ~=? wprob ([]::[String]) waempty1 
      ]

probChoiceTests = 
    [ "choice1"  ~: 2/3 ~=? wprob ["train"] watrainbuschoice,
      "choice2"  ~: 1/3 ~=? wprob ["bus"] watrainbuschoice,
      "choice3"  ~: 1/2 ~=? wprob ["train"] watrainbusbikechoice,
      "choice4"  ~: 1/4 ~=? wprob ["bus"] watrainbusbikechoice,
      "choice5"  ~: 1/4 ~=? wprob ["bike"] watrainbusbikechoice,
      "choice6"  ~: 2/3 ~=? wprob ["a","c"] wachoiceseq,
      "choice7"  ~: 1/3 ~=? wprob ["b","c"] wachoiceseq,
      "choice8"  ~: 0   ~=? wprob ["b","a"] wachoiceseq,
      "choice9"  ~: 1/3 ~=? wprob ["a","c"] wa2choice,
      "choice10" ~: 1/3 ~=? wprob ["a","d"] wa2choice,
      "choice11" ~: 1/6 ~=? wprob ["b","d"] wa2choice,
      "choice12" ~: 2/3 ~=? wprob ["a"] wasilentchoice,
      "choice13" ~: 1/3 ~=? wprob [] wasilentchoice,
      "choice14" ~: 0   ~=? wprob ["b"] wasilentchoice
    ]

probLoopBasicTests = let ?epsilon = testeps in
    [ "loop1"       ~: 2/3  ~=? wprob ["b"] waloop1,
      "loop2"       ~: 2/9  ~?~ wprob ["a","b"] waloop1,
      "loop3"       ~: 2/27 ~?~ wprob ["a","a","b"] waloop1
      ]

probLoopTerminateTests = let ?eps = 0.1 
                             ?epsilon = testeps in
      [ "loopTerm1"   ~: 2/3 ~?~ wprobEps ["b"] waloop1,
        "loopTerm2"   ~: (3/4 + 3/4**2 + 3/4**3 + 3/4**4) 
                ~?~ wprobEps ["b"] wasilloop1
      ]

--


huTests =  probBasicTests 
        ++ probChoiceTests 
        ++ probLoopBasicTests ++ probLoopTerminateTests


-- huTests = probLoopTerminateTests



