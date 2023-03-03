{-# LANGUAGE DeriveDataTypeable #-}
 {-# OPTIONS_GHC -fno-cse #-}

module Main where

import qualified Toothpaste.Binpaste as Binpaste
import Toothpaste.EventLog
import qualified Toothpaste.Flowpaste as Flowpaste hiding (main) 
import Toothpaste.ProcessFormats
import qualified Toothpaste.ProbProcessTree as ProbProcessTree
import Toothpaste.TPConform
import qualified Toothpaste.TPMine as TPMine

import Control.Monad
import Control.Logger.Simple
import Data.List (intercalate,nub)
import Data.Map (Map,lookup)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import System.Console.CmdArgs
import System.Exit
import System.IO

data ToothpasteArgs = 
        ToothpasteArgs{logformat :: String, modeltype :: String,
                       eventlog :: FilePath,
                       pnetfile :: FilePath, ptreefile :: FilePath,
                       ptreeformat :: String,
                       impl :: String,
                       traceprobfile :: FilePath,
                       noise :: Float,
                       verbose :: Bool }
        deriving (Show,Data,Typeable)

data Model = Stochastic | ControlFlow
        deriving (Show,Eq)


data PTreeFormat = PTree | LaTeX
        deriving (Show,Eq)

data Impl = Incr | Binary | MNode
        deriving (Show,Eq)


summaryStr :: String
summaryStr = "Toothpaste Miner 0.9.3.1, 2020-23 (GPL)" 

toothpasteArgs = cmdArgsMode $ ToothpasteArgs{
    logformat = "dt" &= 
        help "Event log format. Valid values dt or dtct", 
    modeltype = "stoch" &= 
        help "Output model type. Valid values stoch or cflow", 
    eventlog  = def &= help "Event log file name. Required.",
    pnetfile  = def &= help "Output Petri Net PNML file. Required.",
    ptreefile = def &= help "Output PPTree file. Required.",
    ptreeformat = "ptree" 
            &= help "Output PPT format. Valid values ptree or latex",
    impl      = "mnode" &= help "Discovery algo. Valid values binary, incr, or mnode. Default mnode.",
    traceprobfile = ""
            &= help "Output trace probabilities to this file",
    noise = def
            &= help "Prune subtrees below this threshold. Range [0..1]. Default 0.",
    verbose = def &= help "Verbose output"
            } 
            &=
    help "Discover stochastic models from event logs" 
        &= summary summaryStr

ptreeOutMain = Binpaste.inputMain


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
                  | istr == "binary" = Binary
                  | istr == "mnode" = MNode
                  | otherwise       = MNode

ptreeformatSelector :: String -> PTreeFormat
ptreeformatSelector istr | istr == "ptree" = PTree
                         | istr == "latex" = LaTeX


-- Binpaste invocation
pptreeIntToStrB :: Binpaste.PPTree Int -> Map Int String 
    -> Binpaste.PPTree String
pptreeIntToStrB (Binpaste.Leaf x n) m       = 
    Binpaste.Leaf (fromJust (Data.Map.lookup x m)) n
pptreeIntToStrB (Binpaste.Silent n) _       = Binpaste.Silent n
pptreeIntToStrB (Binpaste.Node1 op x r n) m = 
    Binpaste.Node1 op (pptreeIntToStrB x m) r n
pptreeIntToStrB (Binpaste.Node2 op x y n) m = 
    Binpaste.Node2 op (pptreeIntToStrB x m) (pptreeIntToStrB y m) n

ptreeOnIntBinary :: ToothpasteArgs -> String -> Binpaste.PPTree String
ptreeOnIntBinary tpargs rawlog = 
                    pptreeIntToStrB (Binpaste.discoverGen intlog) m
        where   strlog     = (parseSelector $ logformat tpargs) rawlog
                (intlog,m) = logIndex strlog


-- Toothpaste invocation (some redundancy with Binpaste)
pptreeIntToStr :: ProbProcessTree.PPTree Int -> Map Int String 
    -> ProbProcessTree.PPTree String
pptreeIntToStr (ProbProcessTree.Leaf x n) m       = 
    ProbProcessTree.Leaf (fromJust (Data.Map.lookup x m)) n
pptreeIntToStr (ProbProcessTree.Silent n) _       = ProbProcessTree.Silent n
pptreeIntToStr (ProbProcessTree.Node1 op x r n) m = 
    ProbProcessTree.Node1 op (pptreeIntToStr x m) r n
pptreeIntToStr (ProbProcessTree.NodeN op ptl n) m = 
    ProbProcessTree.NodeN op (pptreeIntToStrList ptl m) n

pptreeIntToStrList :: [ProbProcessTree.PPTree Int] -> Map Int String 
    -> [ProbProcessTree.PPTree String]
pptreeIntToStrList ptl m = map (`pptreeIntToStr` m) ptl

ptreeOnIntBatch :: ToothpasteArgs -> String -> ProbProcessTree.PPTree String 
ptreeOnIntBatch tpargs rawlog = 
                    pptreeIntToStr (TPMine.discoverGen intlog) m
        where   strlog     = (parseSelector $ logformat tpargs) rawlog
                (intlog,m) = logIndex strlog

ptreeOnIntInc :: ToothpasteArgs -> String -> ProbProcessTree.PPTree String
ptreeOnIntInc tpargs rawlog   = 
                    pptreeIntToStr (TPMine.batchIncDiscover intlog) m
        where   strlog     = (parseSelector $ logformat tpargs) rawlog
                (intlog,m) = logIndex strlog

ptreeOnIntNoise :: ToothpasteArgs -> String -> ProbProcessTree.PPTree String 
ptreeOnIntNoise tpargs rawlog = 
                    pptreeIntToStr (TPMine.discoverNoise intlog nse) m
        where   strlog     = (parseSelector $ logformat tpargs) rawlog
                (intlog,m) = logIndex strlog
                nse        = noise tpargs



-- Flowpaste
ptree ::  ToothpasteArgs -> String -> Flowpaste.PTree String
ptree tpargs = Flowpaste.discover (parseSelector $ logformat tpargs)


-- Trace probability
traceProbForTraces :: Ord a => Log a -> ProbProcessTree.PPTree a 
                                     -> [([a],Float)]
traceProbForTraces alog pptm = map (\trace -> (trace,prob trace pptm) ) 
                                   alog 

traceProbForLog :: Log String -> ProbProcessTree.PPTree String -> String
traceProbForLog strlog pptm  
    = intercalate "\n"  
        (map (\(trace,prob) -> show trace ++ " :: " ++ show prob)  probs)
      ++ "\n" 
      ++ "Sum of trace probabilities: " ++ show totalprob ++ "\n"
      ++ "Probability of empty trace: " ++ show (prob [] pptm ) ++ "\n"
    where   probs      = traceProbForTraces (nub strlog) pptm
            totalprob  = sum $ map snd probs

mine :: ToothpasteArgs -> String -> (String, String)
mine tpargs logtext
    | model == Stochastic && algo == Binary = 
        (Binpaste.formatPPTree pptb, 
         weightedNetToString (Binpaste.translate pptb) "spn" )
    | model == Stochastic && algo == Incr = 
         (pptformatter pptm,
          weightedNetToString (TPMine.translate ppti) "spn" )
    | model == Stochastic && algo == MNode && nse <= 0.0 = 
         (pptformatter pptm, 
          weightedNetToString (TPMine.translate pptm) "spn" ) 
    | model == Stochastic && algo == MNode && nse > 0.0 = 
         (pptformatter pptn, 
          weightedNetToString (TPMine.translate pptn) "spn" ) 
    | model == ControlFlow = 
        (Flowpaste.formatPTree pt,  
         petriNetToString (Flowpaste.translate pt) "pnet" )
    where model  = modelSelector $ modeltype tpargs 
          parser = parseSelector $ logformat tpargs
          algo   = implSelector  $ impl tpargs
          pptf   = ptreeformatSelector $ ptreeformat tpargs
          nse    = noise tpargs
          pptb   = ptreeOnIntBinary tpargs logtext
          ppti   = ptreeOnIntInc    tpargs logtext
          pptm   = ptreeOnIntBatch  tpargs logtext
          pptn   = ptreeOnIntNoise  tpargs logtext
          pt     = Flowpaste.discover  parser logtext
          pptformatter | pptf == PTree = ProbProcessTree.formatPPTree
                       | pptf == LaTeX = ProbProcessTree.latexPPTree


mineWithProb :: ToothpasteArgs -> String -> (String, String, String)
mineWithProb tpargs logtext =
         (pptformatter pptm, 
          weightedNetToString (TPMine.translate pptm) "spn",
          traceProbForLog strlog pptm) 
    where strlog        = (parseSelector $ logformat tpargs) logtext
          (intlog,intm) = logIndex strlog
          pptf          = ptreeformatSelector $ ptreeformat tpargs
          pptm          = pptreeIntToStr (TPMine.discoverGen intlog) intm
          pptformatter | pptf == PTree = ProbProcessTree.formatPPTree
                       | pptf == LaTeX = ProbProcessTree.latexPPTree


main :: IO ()
main = 
    withGlobalLogging (LogConfig Nothing True) $
    do
    tpargs <- cmdArgsRun toothpasteArgs
    when ((null $ eventlog tpargs)  
     || (null $ pnetfile tpargs)
     || (null $ ptreefile tpargs) )
        (do putStr "--eventlog, --pnetfile and --ptreefile are required arguments\n" 
            exitFailure)
    if (verbose tpargs)
        then do 
            setLogLevel LogDebug
            logDebug $ T.pack summaryStr
        else do 
            setLogLevel LogInfo
    inhandle <- openFile (eventlog tpargs) ReadMode
    contents <- hGetContents inhandle
    if null $ traceprobfile tpargs  
        then do 
            let (ptContents,pnetContents) = mine tpargs contents
            writeFile (ptreefile tpargs) ptContents
            writeFile (pnetfile tpargs)  pnetContents
        else do
            let (ptContents,pnetContents,probContents) 
                   = mineWithProb tpargs contents 
            writeFile (ptreefile tpargs) ptContents
            writeFile (pnetfile tpargs)  pnetContents
            writeFile (traceprobfile tpargs) probContents 


