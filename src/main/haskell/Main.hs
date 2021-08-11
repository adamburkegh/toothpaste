{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Data.Map (Map,lookup)
import Data.Maybe (fromJust)
import Toothpaste 
import Flowpaste hiding (main)
import EventLog
import ProcessFormats
import System.Console.CmdArgs
import System.Environment
import System.IO

data ToothpasteArgs = 
        ToothpasteArgs{logformat :: String, modeltype :: String,
                       eventlog :: String,
                       pnetfile :: String, ptreefile :: String,
                       impl :: String } 
        deriving (Show,Data,Typeable)

data Model = Stochastic | ControlFlow
        deriving (Show,Eq)


data Impl = Incr | Batch
        deriving (Show,Eq)

toothpasteArgs = ToothpasteArgs{
    logformat = "dt" &= 
        help "Event log format. Valid values dt or dtct", 
    modeltype = "stoch" &= 
        help "Output model type. Valid values stoch or cflow", 
    eventlog  = ""  &= help "Event log file name",
    pnetfile  = "" &= help "Output Petri Net PNML file",
    ptreefile = "" &= help "Output PPTree file",
    impl      = "batch" &= help "Discovery algo. Valid values batch or incr" } 
        &=
    help "Discover stochastic models from event logs" 
        &= summary "Toothpaste Miner 0.8.4, 2021 (GPL)" 

ptreeOutMain = Toothpaste.inputMain


parseSelector :: String -> String -> Log String
parseSelector pstr | pstr == "dcdt" = parseDCDT
                   | pstr == "dt"   = parseDelimitedTrace
                   | otherwise      = parseDelimitedTrace

modelSelector :: String -> Model
modelSelector mstr | mstr == "stoch" = Stochastic
                   | mstr == "cflow" = ControlFlow
                   | otherwise       = Stochastic

implSelector :: String -> Impl
implSelector istr | istr == "incr"  = Incr
                  | istr == "batch" = Batch
                  | otherwise       = Batch

pptree :: ToothpasteArgs -> String -> PPTree String
pptree tpargs = Toothpaste.discover (parseSelector $ logformat tpargs)

pptreeIntToStr :: PPTree Int -> Map Int String -> PPTree String
pptreeIntToStr (Toothpaste.Leaf x n) m       = 
    Toothpaste.Leaf (fromJust (Data.Map.lookup x m)) n
pptreeIntToStr (Toothpaste.Silent n) _       = Toothpaste.Silent n
pptreeIntToStr (Toothpaste.Node1 op x r n) m = 
    Toothpaste.Node1 op (pptreeIntToStr x m) r n
pptreeIntToStr (Toothpaste.Node2 op x y n) m = 
    Toothpaste.Node2 op (pptreeIntToStr x m) (pptreeIntToStr y m) n


ptreeOnIntBatch :: ToothpasteArgs -> String -> PPTree String
ptreeOnIntBatch tpargs rawlog = 
                    pptreeIntToStr (Toothpaste.discoverGen intlog) m
        where   strlog     = (parseSelector $ logformat tpargs) rawlog
                (intlog,m) = logIndex strlog


ptreeOnIntInc :: ToothpasteArgs -> String -> PPTree String
ptreeOnIntInc tpargs rawlog   = 
                    pptreeIntToStr (Toothpaste.batchIncDiscover intlog) m
        where   strlog     = (parseSelector $ logformat tpargs) rawlog
                (intlog,m) = logIndex strlog

ptree ::  ToothpasteArgs -> String -> PTree String
ptree tpargs = Flowpaste.discover (parseSelector $ logformat tpargs)




mine :: ToothpasteArgs -> String -> (String, String)
mine tpargs logtext
    | model == Stochastic && algo == Batch = 
        (formatPPTree pptb, 
         weightedNetToString (Toothpaste.translate pptb) "spn" )
    | model == Stochastic && algo == Incr = 
         (formatPPTree ppti, 
          weightedNetToString (Toothpaste.translate ppti) "spn" )
    | model == ControlFlow = 
        (formatPTree pt,  petriNetToString (Flowpaste.translate pt) "pnet" )
    where model  = modelSelector $ modeltype tpargs 
          parser = parseSelector $ logformat tpargs
          algo   = implSelector  $ impl tpargs
          -- ppt    = Toothpaste.discover parser logtext
          pptb   = ptreeOnIntBatch tpargs logtext
          ppti   = ptreeOnIntInc   tpargs logtext
          pt     = Flowpaste.discover  parser logtext


main :: IO ()
main = do
    tpargs <- cmdArgs toothpasteArgs
    inhandle <- openFile (eventlog tpargs) ReadMode
    contents <- hGetContents inhandle
    let (ptContents,pnetContents) = mine tpargs contents
    writeFile (ptreefile tpargs) ptContents
    writeFile (pnetfile tpargs)  pnetContents


