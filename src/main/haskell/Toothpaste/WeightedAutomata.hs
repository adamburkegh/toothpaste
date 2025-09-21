{-# LANGUAGE ImplicitParams #-}

module Toothpaste.WeightedAutomata where

import Toothpaste.BaseTypes

import Data.List (nub)
import Data.Set (Set,empty,insert,member,notMember)
import qualified Data.Bimap as BM   -- could be regular Map now

-- import Debug.Trace

-- pretty much a hack instead of (Monoid a) =>
type Activity = String

data Label = Silent | Act Activity
    deriving (Eq, Ord, Show)

instance Semigroup (Label) where
    (<>) Silent Silent = Silent
    (<>) Silent l2     = l2
    (<>) l1 Silent     = l1
    (<>) l1 l2         = l1 <> l2

instance Monoid (Label) where
    mempty = Silent

type State = Int


data EdgeDetails = EdgeDetails {fromState    :: State,
                                toState      :: State,
                                detailLabel  :: Label,
                                edgeId       :: String,
                                edgeWeight   :: Weight}
        deriving (Eq, Ord, Show)


we :: State -> State -> String -> Weight -> String -> EdgeDetails 
we from to label weight edgeid = EdgeDetails from to (Act label) edgeid weight

wes :: State -> State -> Weight -> String -> EdgeDetails 
wes from to weight edgeid = EdgeDetails from to Silent edgeid weight


-- These two operators combine to make edges
-- Eg 1 --< ("train","e1", 1) >-- 2
--
-- lhs edge construction operator
(--<) :: State -> (String,String,Weight) -> (State,Label,String,Weight) 
v1 --< (l1,eid,w) = (v1,Act l1,eid,w)

(--<*) :: State -> (String,Weight) -> (State,Label,String,Weight)
v1 --<* (eid,w) = (v1,Silent,eid,w)

-- rhs edge construction operator
(>--) :: (State,Label,String,Weight) -> State -> EdgeDetails
(v1,l1,eid,w) >-- v2 = EdgeDetails v1 v2 l1 eid w




type EdgeMap = BM.Bimap State [EdgeDetails]

data WSFA = WSFA{ initial    :: Int,
                  final      :: Int,
                  -- intMap     :: IM.AdjacencyIntMap,
                  edgeMap    :: EdgeMap }
           deriving (Show)

wsfaFromList :: State -> State -> [EdgeDetails] -> WSFA
wsfaFromList start end el = WSFA start end edgemap
    where edgemap    = BM.fromList ml
          ml         = zip states stateEdges
          states     = nub ( (map fromState el) ++ (map toState el) )
          stateEdges = map (\v -> filter (\e -> v == (fromState e) ) 
                                         el ) 
                           states


-- default epsilon for approximations
defaulteps = 0.001

wprob :: [Activity] -> WSFA -> Float
wprob s ws = let ?eps = defaulteps in wprobEps s ws

wprobEps :: (?eps :: Float) => [Activity] -> WSFA -> Float 
wprobEps s (WSFA initial final em) = 
    wprobS s (WSFA initial final em) initial ?eps 1 Data.Set.empty


-- wprobS sequence wsfa vertex seen -> prob
-- Should be IntSet State instead of Set?
wprobS :: [Activity] -> WSFA -> State -> Float -> Float -> Set State -> Float
wprobS (a:as) (WSFA initial final em) v eps p seen 
    | wt    == 0     = 0
    | p <= eps     = 
    -- trace ("wprobS a:as p<=eps " ++ (show p) ++ " " ++ (show eps) ) (
        (1/wt) * 
        sum (map (\e -> let w = edgeWeight e in
                        w* wprobS as wsfa (toState e) 
                                  (neweps v seen eps w wt)
                                  (p*w/wt) (insert v seen) )
                 actsucc )  
    | p > eps      =
    -- trace ("wprobS a:as p>eps " ++ (show p) ++ " " ++ (show eps) ) (
        (1/wt) * 
        sum (map (\e -> let w = edgeWeight e in
                        w* wprobS as wsfa (toState e) 
                                  (neweps v seen eps w wt)
                                  (p* w / wt ) (insert v seen) ) 
                 actsucc )
        + (1/wt) *
        sum (map (\e -> let w = edgeWeight e in
                        w* wprobS (a:as) wsfa (toState e) 
                                  (neweps v seen eps w wt)
                                  (p* w / wt ) (insert v seen) )
                 silsucc ) 
    where wsfa      = WSFA initial final em 
          edges     = em BM.! v
          weights   = map (\e -> edgeWeight e) edges
          wt        = sum weights
          actsucc   = chooseEdges (Act a) em edges
          silsucc   = chooseEdges Silent  em edges
wprobS [] (WSFA initial final em) v eps p seen
    | final == v                = 1
    | final /= v && wt == 0     = 0
    | final /= v && p > eps = 
        -- trace ("wprobS [] " ++ (show v) ++ " " ++ (show final) ++ " " ++ (show silsucc) ++ " " ++ (show edges)) ( 
            (1/wt) * 
            sum (map (\e -> let w = edgeWeight e in
                            w* wprobS [] wsfa (toState e) 
                                      (neweps v seen eps w wt)    
                                      (p* w / wt ) (insert v seen) )
                 silsucc )  
    | otherwise                 = 0
    where wsfa      = WSFA initial final em 
          edges     = em BM.! v
          weights   = map (\e -> edgeWeight e) edges
          wt        = sum weights
          silsucc   = chooseEdges Silent em edges


neweps :: State -> Set State -> Float -> Float -> Float -> Float
neweps v seen eps w wt | notMember v seen   = eps * w / wt
                       | member v seen      = eps

chooseEdges :: Label -> EdgeMap -> [EdgeDetails] -> [EdgeDetails]
chooseEdges l1 em edges = 
    filter (\e -> l1 == detailLabel e) edges


