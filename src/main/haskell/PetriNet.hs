module PetriNet where

import ToString
import Data.Set (Set, toList, fromList)
import Data.Typeable

type Weight = Float

-- Petri nets
data Place a = Place {placeName :: a, placeId :: String } deriving (Eq, Ord)
instance (Ord a, Show a, Typeable a) => Show (Place a) where
  show (Place a nodeId) = "p(" ++ toString a  ++ ")"

data Transition a = Transition {transitionName :: a, tranId :: String} 
        deriving (Eq, Ord)
instance (Show a, Typeable a) => Show (Transition a) where
  show (Transition a nodeId) = "t" ++ toString a

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
     = WTransition {wtransitionName :: a, wtranId :: String,
                  tweight :: Weight} 
      deriving (Eq, Ord)
instance (Show a, Typeable a) => Show (WTransition a) where
  show (WTransition a nodeId weight) = "t" ++ toString a 
        ++ ":" ++ show weight 

toTransition :: WTransition a -> Transition a
toTransition wt = Transition (wtransitionName wt) (wtranId wt)  

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



