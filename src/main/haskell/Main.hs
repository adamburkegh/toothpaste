{-# LANGUAGE DeriveDataTypeable #-}

module Main where

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
                       pnetfile :: String, ptreefile :: String } 
        deriving (Show,Data,Typeable)

data Model = Stochastic | ControlFlow
        deriving (Show,Eq)

toothpasteArgs = ToothpasteArgs{
    logformat = "dt" &= 
        help "Event log format. Valid values dt or dtct", 
    modeltype = "stoch" &= 
        help "Output model type. Valid values stoch or cflow", 
    eventlog  = ""  &= help "Event log file name",
    pnetfile  = "" &= help "Output Petri Net PNML file",
    ptreefile = "" &= help ""} &=
    help "Discover stochastic models from event logs" 
        &= summary "Toothpaste Miner 0.7.2, 2021 (GPL)" 

ptreeOutMain = Toothpaste.inputMain


parseSelector :: String -> String -> Log String
parseSelector pstr | pstr == "dcdt" = parseDCDT
                   | pstr == "dt"   = parseDelimitedTrace
                   | otherwise      = parseDelimitedTrace

modelSelector :: String -> Model
modelSelector mstr | mstr == "stoch" = Stochastic
                   | mstr == "cflow" = ControlFlow
                   | otherwise       = Stochastic

pptree :: ToothpasteArgs -> String -> PPTree String
pptree tpargs = Toothpaste.discover (parseSelector $ logformat tpargs)

ptree ::  ToothpasteArgs -> String -> PTree String
ptree tpargs = Flowpaste.discover (parseSelector $ logformat tpargs)

mine :: ToothpasteArgs -> String -> (String, String)
mine tpargs logtext
    | model == Stochastic  = 
        (formatPPTree ppt, 
         weightedNetToString (Toothpaste.translate ppt) "spn" )
    | model == ControlFlow = 
        (formatPTree pt,  petriNetToString (Flowpaste.translate pt) "pnet" )
    where model  = modelSelector $ modeltype tpargs 
          parser = parseSelector $ logformat tpargs
          ppt    = Toothpaste.discover parser logtext
          pt     = Flowpaste.discover  parser logtext


main :: IO ()
main = do
    tpargs <- cmdArgs toothpasteArgs
    inhandle <- openFile (eventlog tpargs) ReadMode
    contents <- hGetContents inhandle
    let (ptContents,pnetContents) = mine tpargs contents
    writeFile (ptreefile tpargs) ptContents
    writeFile (pnetfile tpargs)  pnetContents


