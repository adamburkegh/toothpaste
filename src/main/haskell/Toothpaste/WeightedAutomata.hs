{-# LANGUAGE ImplicitParams #-}

module Toothpaste.WeightedAutomata where

import Toothpaste.BaseTypes

import Data.List (nub)
import Data.Set (Set,empty,insert,member,notMember)
import qualified Data.Bimap as BM   -- could be regular Map now

-- import Debug.Trace

-- pretty much a hack instead of (Monoid a) =>
-- type Activity = String

-- data Label = Silent | Act Activity
--     deriving (Eq, Ord, Show)

-- instance Semigroup (Label) where
--     (<>) Silent Silent = Silent
--     (<>) Silent l2     = l2
--     (<>) l1 Silent     = l1
--     (<>) l1 l2         = l1 <> l2

-- instance Monoid (Label) where
--     mempty = Silent

type State = Int


data EdgeDetails a = EdgeDetails {fromState    :: State,
                                  toState      :: State,
                                  detailLabel  :: Maybe a,
                                  -- eid       :: String TODO remove from fns
                                  edgeWeight   :: Weight}
        deriving (Eq, Ord, Show)



-- These two operators combine to make edges
-- Eg 1 --< ("train","e1", 1) >-- 2
--
-- lhs edge construction operator
(--<) :: (Eq a, Ord a, Show a) => 
    State -> (a,Weight) -> (State,Maybe a,Weight) 
v1 --< (l1,w) = (v1,Just l1,w)

(--<*) :: State -> Weight -> (State,Maybe a,Weight)
v1 --<* w = (v1,Nothing,w)

-- rhs edge construction operator
(>--) :: (State,Maybe a,Weight) -> State -> EdgeDetails a
(v1,l1,w) >-- v2 = EdgeDetails v1 v2 l1 w




type EdgeMap a = BM.Bimap State [EdgeDetails a]

data WSFA a = WSFA{ initial :: State,
                    final   :: State,
                    edgeMap :: EdgeMap a}
           deriving (Eq,Show)

wsfaFromList :: (Eq a, Ord a, Show a) =>
    State -> State -> [EdgeDetails a] -> WSFA a
wsfaFromList start end el = WSFA start end edgemap
    where edgemap    = BM.fromList ml
          ml         = zip states stateEdges
          states     = nub ( (map fromState el) ++ (map toState el) )
          stateEdges = map (\v -> filter (\e -> v == (fromState e) ) 
                                         el ) 
                           states


wprob :: (Eq a, Ord a, Show a) => [a] -> WSFA a -> Float
wprob s ws = let ?eps = defaulteps in wprobEps s ws

wprobEps :: (Eq a, Ord a, Show a, ?eps :: Float) => [a] -> WSFA a -> Float 
wprobEps s (WSFA initial final em) = 
    wprobS s (WSFA initial final em) initial ?eps 1 Data.Set.empty


-- wprobS sequence wsfa vertex seen -> prob
-- Should be IntSet State instead of Set?
wprobS :: (Eq b, Ord b) => 
    [b] -> WSFA b -> State -> Float -> Float -> Set State -> Float
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
          actsucc   = chooseEdges (Just a) em edges
          silsucc   = chooseEdges Nothing  em edges
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
          silsucc   = chooseEdges Nothing em edges


neweps :: State -> Set State -> Float -> Float -> Float -> Float
neweps v seen eps w wt | notMember v seen   = eps * w / wt
                       | member v seen      = eps

chooseEdges :: (Eq a) => 
    Maybe a -> EdgeMap a -> [EdgeDetails a] -> [EdgeDetails a]
chooseEdges l1 em edges = 
    filter (\e -> l1 == detailLabel e) edges


