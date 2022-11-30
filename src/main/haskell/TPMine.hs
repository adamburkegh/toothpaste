module TPMine where

import EventLog
import PetriNet
import ProbProcessTree
import Toothpaste (transform,transformNoise)
import Debug.Trace
import Data.List (sort)
import Data.Set (fromList,union,unions)

-- debug and trace
debug :: String -> a -> a
-- Debug ON
debug = trace

-- Debug OFF
-- debug x y = y

warn msg = trace ("Warn: " ++ msg)


-- Mining
traceModel :: (Ord a, Eq a) => Log a -> PPTree a
traceModel lg = NodeN Choice ul ulw
    where ul  = traceConsolidate $ sort lg
          ulw = sum (map weight ul)

traceConsolidate :: (Eq a) => Log a -> [PPTree a]
traceConsolidate (t1:t2:lg) 
    | t1 == t2 = tracePPTree 2 t1:traceConsolidate lg
    | t1 /= t2 = tracePPTree 1 t1:traceConsolidate (t2:lg)
traceConsolidate [t] = [tracePPTree 1 t]
traceConsolidate []  = []

tracePPTree :: Weight -> Trace a -> PPTree a
tracePPTree rf t = NodeN Seq (map (`Leaf` rf) t) rf

discover :: Parser -> String -> PPTree String
discover parser = transform . traceModel . parser

discoverGen :: (Ord a, Eq a, Show a) => Log a -> PPTree a
discoverGen log = transform $ traceModel log

discoverNoise :: (Ord a, Eq a, Show a) => Log a -> Float -> PPTree a
discoverNoise log = transformNoise (traceModel log) 


-- Petri net conversion
-- Limited to Petri nets of Strings

translate :: PPTree String -> WeightedNet
translate ptree 
    | valResult val = net
    | otherwise       = warn ( valMsg val ) net
    where net =  ptreeWeightedNet ptree (Place "I" "pI") (Place "O" "pO") 1
          val = validateWeightedNet net     

nextid :: Int -> String
nextid x = "t" ++ show (x+1)

midp :: Int -> Place String
midp newId = Place "" ("p" ++ show newId)


-- ptreeWeightedNet PPTree initialPlace finalPlace idOffset
-- pre: no operator nodes with empty child lists
ptreeWeightedNet :: PPTree String -> Place String -> Place String -> Int
            -> WeightedNet

ptreeWeightedNet (NodeN Choice ptl w) pi po idp =
    let ptlr = ptreeWeightedNetChoiceList ptl pi po (idp+1)
    in WeightedNet (unions (map wnplaces ptlr))
                   (unions (map wntransitions ptlr))
                   (unions (map wnedges ptlr))
        pi po (wnmaxnodeid (last ptlr))

ptreeWeightedNet (Node1 FLoop x m w) pi po idp
    = ptreeWeightedNet (seqP (replicate (round m) x) w ) pi po idp

ptreeWeightedNet (Node1 PLoop x m w) pi po idp =
    let midp1 = midp (idp+1)
        trantauin  = WTransition "tauin"  (nextid (idp+2)) w
        trantauout = WTransition "tauout" (nextid (idp+3)) (w/m)
        px      =   ptreeWeightedNet (scale x ((m-1)/m) ) 
                                     midp1 midp1 ( idp+4 )
    in WeightedNet (wnplaces px `union` fromList [midp1,pi,po] )
                   (wntransitions px `union` fromList [trantauin,trantauout] )
                   (wnedges px `union`
                        fromList [WToTransition pi trantauin,
                                   WToPlace trantauin midp1,
                                   WToTransition midp1 trantauout,
                                   WToPlace trantauout po ]  )
                   pi po (wnmaxnodeid px)

ptreeWeightedNet (NodeN Seq ptl w) pi po idp =
        let ptlr = ptreeWeightedNetSeqList ptl pi po idp
        in WeightedNet (unions (map wnplaces ptlr))
                       (unions (map wntransitions ptlr))
                       (unions (map wnedges ptlr))
                   pi po (wnmaxnodeid (last ptlr))

ptreeWeightedNet (NodeN Conc ptl w) pi po idp =
    let ptlr = ptreeWeightedNetConcList ptl trantauin trantauout (idp+2)
        trantauin  = WTransition "tau" (nextid idp) w
        trantauout = WTransition "tau" (nextid (idp+1)) w
    in WeightedNet (unions (map wnplaces ptlr 
                           ++ [fromList[pi,po]]))
                   (unions (map wntransitions ptlr
                           ++ [fromList[trantauin,trantauout]]))
                   (unions (map wnedges ptlr
                           ++ [fromList [WToTransition pi trantauin,
                                        WToPlace trantauout po]]))
        pi po (wnmaxnodeid (last ptlr))

ptreeWeightedNet (Leaf x w) pi po idp =
        let tx = WTransition x (nextid idp) w
        in WeightedNet (fromList[pi,po]) (fromList[tx])
                       (fromList [WToTransition pi tx, WToPlace tx po] )
                       pi po (idp+1)

ptreeWeightedNet (Silent w) pi po idp
    = ptreeWeightedNet (Leaf tau w) pi po (idp+1)

-- ptreelist in out idoffset
ptreeWeightedNetChoiceList :: [PPTree String] -> Place String 
    -> Place String -> Int -> [WeightedNet]
ptreeWeightedNetChoiceList (pt:ptl) pi po idp = 
    ph:ptreeWeightedNetChoiceList ptl pi po (wnmaxnodeid ph) 
    where ph = ptreeWeightedNet pt pi po idp
ptreeWeightedNetChoiceList [] pi po idp       = []

ptreeWeightedNetSeqList :: [PPTree String] -> Place String 
    -> Place String -> Int -> [WeightedNet]
ptreeWeightedNetSeqList (pt1:pt2:ptl) pi po idp = 
    ph:ptreeWeightedNetSeqList (pt2:ptl) midp1 po (wnmaxnodeid ph) 
    where ph    = ptreeWeightedNet pt1 pi midp1 (idp+1)
          midp1 = midp (idp+1)
ptreeWeightedNetSeqList [pt] pi po idp =
    [ptreeWeightedNet pt pi po idp]
ptreeWeightedNetSeqList [] pi po idp       = []

-- ptreelist tauin tauout idoffset
-- these nets must be linked to the start and end places in the caller 
-- to be valid
ptreeWeightedNetConcList :: [PPTree String] -> WTransition String 
    -> WTransition String -> Int -> [WeightedNet]
ptreeWeightedNetConcList (pt:ptl) ti to idp = 
    php:ptreeWeightedNetConcList ptl ti to mxid
    where iph = midp (idp+1)
          oph = midp (idp+2)
          ph    = ptreeWeightedNet pt iph oph (idp+2)
          mxid  = wnmaxnodeid ph
          php   = WeightedNet (wnplaces ph)
                              (wntransitions ph)
                              (wnedges ph `union` 
                                    fromList [WToPlace ti iph, 
                                              WToTransition oph to])
                              iph oph mxid
ptreeWeightedNetConcList [] pi po idp       = []



