module Toothpaste where

import PetriNet -- mainly for Weight
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
ptl1 =$= ptl2 = length ptl1  == length ptl2  
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
    = NodeN op1 (foldr (\(x,y) c -> merge x y:c) [] (zip ptl1 ptl2))  
                (w1+w2)

-- Pretty printing
indentStr = "  "

formatWeight :: Weight -> String
formatWeight n = ":" ++ show n ++ "\n"

formatPPTree :: (Show a) => PPTree a -> String
formatPPTree x = formatPPTreeIndent x 0

formatPPTreeIndent :: (Show a) => PPTree a -> Int -> String
formatPPTreeIndent (Leaf x n) indent =
    duplicate indentStr indent ++ show x ++ formatWeight n
formatPPTreeIndent (Silent n) indent =
    duplicate indentStr indent ++ tau ++ formatWeight n
formatPPTreeIndent (Node1 op x r n) indent =
    duplicate indentStr indent ++ show op ++ "[" ++ show r ++ "]"
        ++ formatWeight n
        ++ formatPPTreeIndent x (indent+1)
formatPPTreeIndent (NodeN op ptl n) indent =
    duplicate indentStr indent ++ show op  ++ formatWeight n
        ++ (foldl (++) "" 
                (map (\pt -> formatPPTreeIndent pt (indent+1) ) ptl ) )

duplicate string n = concat $ replicate n string


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
choiceSimList (u1:u2:ptl) | u1 =~= u2 = choiceSimList (merge u1 u2:ptl)
                          | otherwise = u1 : choiceSimList (u2:ptl)
choiceSimList x = x

concSim :: Eq a => PRule a
concSim (NodeN Conc ptl w)
    | ptl /= cr = NodeN Conc cr w
    where cr = concSimList ptl
concSim x = x

concSimList :: (Eq a) => [PPTree a] -> [PPTree a]
concSimList (u1:u2:ptl) 
    | u1 =~= u2 = concSimList (Node1 FLoop (merge u1 u2) 2 (w1+w2):ptl)
    | otherwise = u1:concSimList (u2:ptl)
        where w1 = weight u1 
              w2 = weight u2
concSimList x = x

-- no loops of subseq >= 2
fixedLoopRoll :: Eq a => PRule a
fixedLoopRoll (NodeN Seq (u1:ptl) w) 
    | nptl /= ptl = NodeN Seq nptl w
    where nptl = fixedLoopRollList ptl u1 1
fixedLoopRoll x = x    

fixedLoopRollList :: (Eq a) => [PPTree a] -> PPTree a -> Int -> [PPTree a]
fixedLoopRollList (u1:ptl) prev ct 
    | u1 == prev            = fixedLoopRollList ptl prev (ct+1)
    | u1 /= prev && ct > 1  = 
            (Node1 FLoop prev (fromIntegral ct) (weight prev)):
                        (fixedLoopRollList ptl u1 1)
    | u1 /= prev && ct <= 1 = prev:(fixedLoopRollList ptl u1 1)
fixedLoopRollList [] prev ct 
    | ct  > 1 = [Node1 FLoop prev (fromIntegral ct) (weight prev)]
    | ct <= 1 = [prev]


-- no loops of subseq >= 2
probLoopRoll :: Eq a => PRule a
probLoopRoll (NodeN Seq (u1:ptl) w) 
    | nptl /= ptl = NodeN Seq nptl w
    where nptl = probLoopRollList ptl u1 1
probLoopRoll x = x    

probLoopRollList :: (Eq a) => [PPTree a] -> PPTree a -> Int -> [PPTree a]
probLoopRollList (u1:ptl) prev ct 
    | u1 == prev            = probLoopRollList ptl prev (ct+1)
    | u1 /= prev && ct > 1  = 
            (Node1 PLoop prev (fromIntegral ct) (weight prev)):
                        (probLoopRollList ptl u1 1)
    | u1 /= prev && ct <= 1 = prev:(probLoopRollList ptl u1 1)
probLoopRollList [] prev ct 
    | ct  > 1 = [Node1 PLoop prev (fromIntegral ct) (weight prev)]
    | ct <= 1 = [prev]

loopFixToProb :: PRule a
loopFixToProb (Node1 FLoop x m w) = Node1 PLoop x m w
loopFixToProb x = x

loopNest :: PRule a
loopNest (Node1 FLoop (Node1 FLoop x r1 w1) r2 w2) = Node1 FLoop x (r1*r2) w2
loopNest (Node1 FLoop (Node1 PLoop x rp wp) rf wf)
    | rf > 1 = Node1 PLoop x (rp*(rf-1)) wf
loopNest (Node1 PLoop (Node1 op x rf wf) rp wp)
    | op == FLoop || op == PLoop = Node1 PLoop x (rf*rp) wp
loopNest x = x

loopGeo :: (Eq a) => PRule a
loopGeo (NodeN Choice ptl w)
    | ptl /= cr = NodeN Choice cr w
    where cr = loopGeoList ptl
loopGeo x = x

loopGeoList :: (Eq a) => [PPTree a] -> [PPTree a]
loopGeoList ((Node1 FLoop u1 r1 w1):(Node1 FLoop u2 r2 w2):ptl) 
    | u1 =~= u2 = loopGeoList  (
                    (Node1 PLoop (merge u1 u2) 
                                 (((r1*w1)+(r2*w2))/(w1+w2)) 
                                 (w1+w2) )
                    :ptl)
    | otherwise = u1 : loopGeoList (u2:ptl)
loopGeoList x = x


flatten :: (Eq a) => PRule a
flatten (NodeN op1 ptl w) = NodeN op1 (flattenList op1 ptl) w
flatten x = x

flattenList :: (Eq a) => POperN -> [PPTree a] -> [PPTree a]
flattenList op1 ((NodeN op2 ptl2 w2):ptl1)
    | op1 == op2 = ptl2 ++ flattenList op1 ptl1
    | otherwise  = NodeN op2 ptl2 w2:ptl1
flattenList op1 (pt:ptl) = pt:flattenList op1 ptl
flattenList op1 [] = []


seqPrefixMerge :: (Eq a) => [PPTree a] -> [PPTree a]
seqPrefixMerge ((NodeN Seq (pt1:ptl1) w1):(NodeN Seq (pt2:ptl2) w2):ptl)
    | pt1 =~= pt2 && (ptl1 /= [] || ptl2 /= [])
          = ((NodeN Seq [(merge pt1 pt2),
                       (NodeN Choice [NodeN Seq ptl1 w1,
                                      NodeN Seq ptl2 w2] nw)] nw):
                     (seqPrefixMerge ptl))
    | pt1 =~= pt2 && (ptl1 == [] && ptl2 == []) 
         = (merge pt1 pt2):(seqPrefixMerge ptl)
    | otherwise = (NodeN Seq (pt1:ptl1) w1):
                  (seqPrefixMerge ((NodeN Seq (pt2:ptl2) w2):ptl))
    where nw = (w1+w2)
seqPrefixMerge ptl = ptl

choiceFoldPrefix :: (Eq a, Ord a) => PRule a
choiceFoldPrefix (NodeN Choice ptl w)
    | ptl /= nptl = NodeN Choice nptl w
        where nptl = seqPrefixMerge ptl
choiceFoldPrefix x = x

-- Warning last is O(N) on lists
seqSuffixMerge :: (Eq a) => [PPTree a] -> [PPTree a]
seqSuffixMerge ((NodeN Seq ptl1 w1):(NodeN Seq ptl2 w2):ptl)
    | pt1 =~= pt2 = ((NodeN Seq [(NodeN Choice [NodeN Seq nptl1 w1,
                                                NodeN Seq nptl2 w2] nw),
                                 merge pt1 pt2] nw):
                     (seqSuffixMerge ptl))
    | otherwise = (NodeN Seq ptl1 w1):
                  (seqSuffixMerge ((NodeN Seq ptl2 w2):ptl))
     where pt1 = last ptl1
           pt2 = last ptl2
           nptl1 = fst $ splitAt ((length ptl1)-1) ptl1
           nptl2 = fst $ splitAt ((length ptl2)-1) ptl2
           nw = w1+w2
seqSuffixMerge ptl = ptl

-- duplication across prefix suffix folds and maybe other choice
choiceFoldSuffix :: (Eq a, Ord a) => PRule a
choiceFoldSuffix (NodeN Choice ptl w)
    | ptl /= nptl = NodeN Choice nptl w
        where nptl = seqSuffixMerge ptl
choiceFoldSuffix x = x



-- Rule lists

baseRuleList :: (Eq a, Ord a) => [TRule a]
baseRuleList = [
            TRule{rulename="singleNodeOp",trule=singleNodeOp},
            TRule{rulename="flatten",trule=flatten},
            TRule{rulename="silentSeq",trule=silentSeq},
            TRule{rulename="fixedLoopRoll", trule=fixedLoopRoll},
            TRule{rulename="choiceSim",trule=choiceSim},
            TRule{rulename="concSim",trule=concSim},
            TRule{rulename="choiceFoldPrefix",trule=choiceFoldPrefix},
            TRule{rulename="choiceFoldSuffix",trule=choiceFoldSuffix},
            TRule{rulename="loopNest",trule=loopNest},
            TRule{rulename="loopGeo",trule=loopGeo},
            TRule{rulename="loopFixToProb", trule=loopFixToProb}
            ]

ruleList :: (Show a, Eq a, Ord a) => [TRule a]
ruleList = baseRuleList

-- Rule application

transform :: (Show a, Eq a, Ord a) => PPTree a -> PPTree a
transform = transformRuleOrdered

-- transformClean x = maxTransformBreadth x ruleList 
transformRuleOrdered x = maxTransformRuleOrder x ruleList

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



vrule :: (Show a, Eq a) => PPTree a -> TRule a -> PPTree a
vrule x r = trule r x
-- validation disabled
-- vrule x r = validateDebug x y r y
--     where y = trule r x

transformPT :: (Show a, Eq a) => PPTree a -> TRule a -> PPTree a
transformPT (Node1 op x rp w) rl = vrule (Node1 op (transformPT x rl) rp w) rl
transformPT (NodeN op ptl w) rl =
    vrule (NodeN op (map (`transformPT` rl) ptl) w) rl
transformPT pt rl = vrule pt rl


ncount :: PPTree a -> Int
ncount (Leaf a _) = 1
ncount (Silent _) = 1
ncount (Node1 op a _ _) = 1 + ncount a
ncount (NodeN op ptl _)  = 1 + foldl (\c pt -> c + ncount pt) 0 ptl



