module Toothpaste.PetriNet where

import Toothpaste.ToString
import Data.Set (Set, toList, fromList,difference,empty)
import Data.Typeable

type Weight = Float

-- Petri nets
data Place a = Place {placeName :: a, placeId :: String } deriving (Eq, Ord)
instance (Ord a, Show a, Typeable a) => Show (Place a) where
  show (Place a nodeId) = "p(" ++ toString a  ++ ")"

data Transition a = Transition {transitionName :: a, 
                                tranId :: String }
                   | SilentTransition {tranId :: String }
        deriving (Eq, Ord)
instance (Show a, Typeable a) => Show (Transition a) where
  show (Transition a nodeId) = "t" ++ toString a
  show (SilentTransition nodeId) = "tau"

data Edge a = ToPlace (Transition a) (Place a) 
            | ToTransition (Place a) (Transition a) deriving (Eq, Ord)
instance (Ord a, Show a, Typeable a) => Show (Edge a) where
  show (ToPlace a b) =  toString a ++ " -> " ++ toString b 
  show (ToTransition a b) = toString a ++ " -> " ++ toString b 

type PNNode a = Either (Place a) (Transition a)

type PetriNet a = (Set (Place a), Set (Transition a), Set (Edge a))

data WorkflowNet = WorkflowNet { places :: Set (Place String),
                                 transitions :: Set (Transition String),
                                 edges :: Set (Edge String),
                                 initial :: Place String,
                                 final :: Place String,
                                 maxnodeid :: Int } deriving (Show,Eq)
-- Could add static constraint that input /= output and both are in places

-- We could probably have a better type relationship
-- Fair bit of repetition
data WTransition a  
     = WTransition {wtransitionName :: a, 
                    wtranId :: String,
                    tweight :: Weight,
                    wsilent :: Bool} 
      deriving (Eq, Ord)
instance (Show a, Typeable a) => Show (WTransition a) where
  show (WTransition a nodeId weight sil) = "t" ++ toString a 
        ++ ":" ++ show weight 

wtransition :: a -> String -> Weight -> WTransition a
wtransition name tranId w = WTransition name tranId w False

silentTransition :: a -> String -> Weight -> WTransition a
silentTransition name tranId w = WTransition name tranId w True


toTransition :: WTransition a -> Transition a
toTransition wt | wsilent wt = SilentTransition (wtranId wt)  
                | otherwise  = Transition (wtransitionName wt) (wtranId wt) 

data WEdge a = WToPlace (WTransition a) (Place a) 
             | WToTransition (Place a) (WTransition a) deriving (Eq, Ord)
instance (Ord a, Show a, Typeable a) => Show (WEdge a) where
  show (WToPlace a b) =  toString a ++ " -> " ++ toString b 
  show (WToTransition a b) = toString a ++ " -> " ++ toString b 

toEdge :: WEdge a -> Edge a
toEdge (WToPlace wt p)      = ToPlace (toTransition wt) p
toEdge (WToTransition p wt) = ToTransition p (toTransition wt)

-- A WorkflowNet with weights, or a GSPN with only immediate transitions,
-- and WorkflowNet constraints, if you prefer
data WeightedNet 
    = WeightedNet { wnplaces :: Set (Place String),
                    wntransitions :: Set (WTransition String),
                    wnedges :: Set (WEdge String),
                    wninitial :: Place String,
                    wnfinal :: Place String,
                    wnmaxnodeid :: Int } deriving (Show,Eq)
-- input /= output and both are in places



toPetriNet :: WeightedNet -> PetriNet String
toPetriNet wnet = (wnplaces wnet, 
                  fromList $ 
                       map toTransition (toList (wntransitions wnet)),
                  fromList $ 
                       map toEdge ( toList (wnedges wnet) ) )

-- General validation type. Not very PetriNet specific
data Validation = Validation{valResult::Bool, valMsg:: String}
    deriving (Show,Eq)
valOk = Validation{valResult=True, valMsg="Ok"}

valFail :: String -> Validation
valFail msg = Validation{valResult=False, valMsg=msg}

isTranSource :: WEdge a -> Bool
isTranSource (WToPlace tran place) = True
isTranSource x = False

isTranTarget :: WEdge a -> Bool
isTranTarget (WToTransition place tran) = True
isTranTarget x = False

validateWeightedNet :: WeightedNet -> Validation
validateWeightedNet wnet 
    | sdiff /= empty  
        = valFail (valFailInit 
                ++ "Transitions without sources:" 
                ++ show sdiff ) 
    | tdiff /= empty  
        = valFail (valFailInit
                ++ "Transitions without targets:" 
                ++ show tdiff ) 
    | otherwise       = valOk
    where trans       = wntransitions wnet
          edg         = toList (wnedges wnet)
          sourcetrans = fromList [ tran | WToPlace tran place <- edg ]
          targettrans = fromList [ tran | WToTransition place tran <- edg ]
          sdiff       = trans `difference` sourcetrans
          tdiff       = trans `difference` targettrans
          valFailInit = "Validation failed for net. "


debugTransition :: WTransition String -> String
debugTransition (WTransition a nodeId weight vis) 
    = "t" ++ a ++ ":" ++ show weight ++ "[" ++ nodeId ++ "]"

debugPlace :: Place String -> String
debugPlace (Place a nodeId) = "p(" ++ a  ++  "[" ++ nodeId ++ "] )"  

debugEdge :: WEdge String -> String
debugEdge (WToPlace a b)      = debugTransition a ++ " -> " ++ debugPlace b 
debugEdge (WToTransition a b) = debugPlace a ++ " -> " ++ debugTransition b 

formatWNetDebug :: WeightedNet -> String
formatWNetDebug wnet =
       "Places: "  ++ unwords ( map debugPlace (toList (wnplaces wnet)) ) 
    ++ " Transitions: " 
    ++ unwords ( map debugTransition (toList(wntransitions wnet)) )
    ++ " Edges: " 
    ++ unwords ( map debugEdge (toList (wnedges wnet) ) )


