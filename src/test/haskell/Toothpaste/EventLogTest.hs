module Toothpaste.EventLogTest where

import Toothpaste.EventLog
import Data.Map (Map,empty,fromList)

import System.Exit
import Test.HUnit

traceIndexTests = [
    "traceIndexEmpty" ~: empty ~=? traceIndex ([]::[String]),
    "traceIndexSingle" ~: fromList [("a",1)] ~=? traceIndex ["a"],
    "traceIndexMulti" ~: fromList [("a",1),("b",2),("c",3)] 
                ~=? traceIndex ["a","b","b","c"] 
    ]


logIndexTests = [
    "logIndexEmpty" ~: ([[]],empty) ~=? logIndex ([[]]::[[String]]),
    "logIndexSingle" ~: ([[1]], fromList [(1,"a")]) ~=? logIndex [["a"]],
    "logIndexSingleTrace" ~: ([[1,2]], fromList [(1,"a"),(2,"b")] )
                                ~=? logIndex [["a","b"]],
    "logIndexMulti" ~: ([[1,2],[3,1,1,2]], 
                            fromList [(1,"b"),(2,"c"),(3,"a")] ) 
                ~=? logIndex [["b","c"], ["a","b","b","c"]],
    "logIndexDupe"  ~: ([[1,2],[1,2]], 
                            fromList [(1,"a"),(2,"b")])
                ~=? logIndex [["a","b"], ["a","b"]]
    ]


huTests = traceIndexTests ++ logIndexTests


