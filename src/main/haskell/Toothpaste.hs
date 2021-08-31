module Toothpaste where

import PetriNet
import Debug.Trace
import Data.Set (fromList,union,unions)

-- debug and trace
debug :: String -> a -> a
-- Debug ON
debug = trace

-- Debug OFF
-- debug x y = y


-- Process Trees

type Repeat = Float

data PPTree a = Leaf a Weight
               | Silent Weight
               | Node1 POper1 (PPTree a) Repeat Weight
               | NodeN POperN [PPTree a] Weight
                deriving (Show,Ord)


data POperN = Choice | Seq | Conc deriving (Enum,Show,Eq,Ord)
data POper1 = PLoop | FLoop deriving (Enum,Show,Eq,Ord)
data POper = POper1 | POper2

silent :: Weight -> PPTree String
silent = Silent


instance (Eq a) => Eq (PPTree a) where
    Leaf a n == Leaf b m = a == b && n == m
    Silent n == Silent m = n == m
    Node1 p1 a r1 n == Node1 p2 b r2 m 
        = (p1 == p2) && (a == b) && (r1 == r2) && (n == m)
    NodeN po1 ptl1 n == NodeN po2 ptl2 m 
        = (po1 == po2) && (ptl1 == ptl2) && (n == m)
    x == y = False


-- Structural equality aka similarity
-- No hash for labels so may be expensive in this impl, esp for large trees
(=~=) :: (Eq a) => PPTree a -> PPTree a -> Bool
(Leaf a n) =~= (Leaf b m)   = a == b
(Silent n) =~= (Silent m)   = True
(Node1 op1 x r1 w1) =~= (Node1 op2 y r2 w2)  
    | op1 == PLoop = op1 == op2 && x =~= y
    | op1 == FLoop = op1 == op2 && r1 == r2 && x =~= y
(NodeN op1 ptl1 w1) =~= (NodeN op2 ptl2 w2)
    = op1 == op2 && ptl1 =$= ptl2 
x =~= y = False

(=$=) :: (Eq a) => [PPTree a] -> [PPTree a] -> Bool
ptl1 =$= ptl2 = length(ptl1) == length(ptl2) 
        && foldl (\c (pt1,pt2) -> (pt1 =~= pt2) && c) True z
    where z = zip ptl1 ptl2


weight :: PPTree a -> Weight
weight (Leaf x n) = n
weight (Silent n) = n
weight (Node1 op x r n) = n
weight (NodeN op ptl n) = n


-- Careful using this one - it breaks consistency and can produce invalid trees
-- Currently only used in converting a loop to a PetriNet - not from rules
replaceWeight :: PPTree a -> Weight -> PPTree a
replaceWeight (Leaf x n) w = Leaf x w
replaceWeight (Silent n) w = Silent w
replaceWeight (Node1 op x r n) w = Node1 op x r w
replaceWeight (NodeN op ptl n) w = NodeN op ptl w


-- tau = "\120591"
tau = "tau" -- encoding issues for Haskell on windows, TODO

emptyTree :: PPTree a
emptyTree = Silent 0

epsilon = 0.0001

-- pre: input trees are structurally equal
-- note implementation always takes lhs for efficiency
merge :: PPTree a -> PPTree a -> PPTree a
merge (Leaf x w1) (Leaf y w2) = Leaf x (w1+w2)
merge (Silent w1) (Silent w2) = Silent (w1+w2)
merge (Node1 FLoop x r1 w1) (Node1 op2 y r2 w2)
   = Node1 FLoop (merge x y) r1 (w1+w2)
merge (Node1 PLoop x r1 w1) (Node1 op2 y r2 w2)
   = Node1 PLoop (merge x y) ((w1*r1+w2*r2)/(w1+w2)) (w1+w2)
merge (NodeN op1 ptl1 w1) (NodeN op2 ptl2 w2)
    = NodeN op1 (foldr (\(x,y) c -> (merge x y):c) [] (zip ptl1 ptl2))  
                (w1+w2)


-- Rule types

type PRule a = PPTree a -> PPTree a

data TRule a = TRule{ rulename :: String, trule :: PRule a }

type PPTRuleTransform a = PPTree a -> [TRule a] -> PPTree a


-- Rules

silentSeq :: PRule a
silentSeq (NodeN Seq ((Silent _):pts) w) = silentSeq (NodeN Seq pts w)
silentSeq (NodeN Seq (pt:pts) w)         = NodeN Seq (pt:ptsr) w
            where NodeN Seq ptsr w2 = silentSeq (NodeN Seq pts w)
silentSeq x = x

singleNodeOp :: PRule a
singleNodeOp (NodeN op [u] w)  = u
singleNodeOp x = x

choiceSim :: (Eq a) => PRule a
choiceSim (NodeN Choice ptl w)
    | ptl /= cr  = NodeN Choice cr w
    where cr = choiceSimList ptl
choiceSim x = x

choiceSimList :: (Eq a) => [PPTree a] -> [PPTree a]
choiceSimList (u1:u2:ptl) | u1 =~= u2 = choiceSimList ((merge u1 u2):ptl)
                         | otherwise = u1:(choiceSimList (u2:ptl))
choiceSimList x = x

concSim :: Eq a => PRule a
concSim (NodeN Conc ptl w)
    | ptl /= cr = NodeN Conc cr w
    where cr = concSimList ptl
concSim x = x

concSimList :: (Eq a) => [PPTree a] -> [PPTree a]
concSimList (u1:u2:ptl) 
    | u1 =~= u2 = concSimList ((Node1 FLoop (merge u1 u2) 2 (w1+w2)):ptl)
    | otherwise = u1:(concSimList (u2:ptl))
        where w1 = weight u1 
              w2 = weight u2
concSimList x = x

flatten :: (Eq a) => PRule a
flatten (NodeN op1 ptl w) = NodeN op1 (flattenList op1 ptl) w
flatten x = x

flattenList :: (Eq a) => POperN -> [PPTree a] -> [PPTree a]
flattenList op1 ((NodeN op2 ptl2 w2):ptl1)
    | op1 == op2 = ptl2 ++ (flattenList op1 ptl1)
    | otherwise  = (NodeN op2 ptl2 w2):ptl1
flattenList op1 (pt:ptl) = pt:(flattenList op1 ptl)
flattenList op1 [] = []


-- Rule lists

baseRuleList :: (Eq a, Ord a) => [TRule a]
baseRuleList = [
            TRule{rulename="silentSeq",trule=silentSeq},
            TRule{rulename="singleNodeOp",trule=singleNodeOp},
            TRule{rulename="choiceSim",trule=choiceSim},
            TRule{rulename="concSim",trule=concSim}
            ]

ruleList :: (Show a, Eq a, Ord a) => [TRule a]
ruleList = baseRuleList

-- Rule application
transformInRuleOrder :: (Show a, Eq a) => PPTRuleTransform a
transformInRuleOrder pt [r] = transformPT pt r
transformInRuleOrder pt (r:rs) =
    transformInRuleOrder (transformInRuleOrder pt [r]) rs
transformInRuleOrder x _ = x

maxTransformRuleOrder :: (Show a, Eq a) => PPTRuleTransform a
maxTransformRuleOrder x rules | x == y      = x
                     | otherwise   = maxTransformRuleOrder y rules
                     where y = debug ("=== Count:" ++ show (ncount x)
                                    ++  "===") (transformInRuleOrder x rules)


transformRuleOrdered x = maxTransformRuleOrder x ruleList

vrule :: (Show a, Eq a) => PPTree a -> TRule a -> PPTree a
vrule x r = trule r x
-- validation disabled
-- vrule x r = validateDebug x y r y
--     where y = trule r x

transformPT :: (Show a, Eq a) => PPTree a -> TRule a -> PPTree a
transformPT (Node1 op x rp w) rl = vrule (Node1 op (transformPT x rl) rp w) rl
transformPT (NodeN op ptl w) rl =
    vrule (NodeN op (map (\pt -> transformPT pt rl) ptl) w) rl
transformPT pt rl = vrule pt rl


ncount :: PPTree a -> Int
ncount (Leaf a _) = 1
ncount (Silent _) = 1
ncount (Node1 op a _ _) = 1 + ncount a
ncount (NodeN op ptl _)  = 1 + foldl (\c pt -> c + ncount pt) 0 ptl


-- Petri net conversion
-- Limited to Petri nets of Strings

translate :: PPTree String -> WeightedNet
translate ptree = ptreeWeightedNet ptree (Place "I" "pI") (Place "O" "pO") 1

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
    | m <= 1 = ptreeWeightedNet x pi po idp
    | m > 1  =
        let midp1 = midp (idp+1)
            px      =   ptreeWeightedNet x pi midp1 ( idp+2 )
            nx      =   ptreeWeightedNet (Node1 FLoop x (m-1) w) midp1 po
                                                        ( wnmaxnodeid px )
        in WeightedNet (unions [(wnplaces px),(wnplaces nx),
                               (fromList [midp1,pi,po])]) 
                   (wntransitions px `union` wntransitions nx)
                   (wnedges px `union` wnedges nx)
                   pi po (wnmaxnodeid px)

ptreeWeightedNet (Node1 PLoop x m w) pi po idp =
    let midp1 = midp (idp+1)
        trantauin  = WTransition "tauin"  (nextid (idp+2)) w
        trantauout = WTransition "tauout" (nextid (idp+3)) 1
        px      =   ptreeWeightedNet (replaceWeight x m) midp1 midp1 ( idp+4 )
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
        trantauout = WTransition "tau" (nextid (idp+1)) 1
    in WeightedNet (unions ((map wnplaces ptlr) 
                           ++ [fromList[pi,po]]))
                   (unions ((map wntransitions ptlr)
                           ++ [fromList[trantauin,trantauout]]))
                   (unions ((map wnedges ptlr)
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
    ph:(ptreeWeightedNetChoiceList ptl pi po (wnmaxnodeid ph) )
    where ph = ptreeWeightedNet pt pi po idp
ptreeWeightedNetChoiceList [] pi po idp       = []

ptreeWeightedNetSeqList :: [PPTree String] -> Place String 
    -> Place String -> Int -> [WeightedNet]
ptreeWeightedNetSeqList (pt1:pt2:ptl) pi po idp = 
    ph:(ptreeWeightedNetSeqList (pt2:ptl) midp1 po (wnmaxnodeid ph) )
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
    php:(ptreeWeightedNetConcList ptl ti to mxid)
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



