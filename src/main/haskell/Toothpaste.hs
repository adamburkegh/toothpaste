module Toothpaste where

import PetriNet -- mainly for Weight
import Debug.Trace
import Data.List (nub,sort,sortOn)
import Data.Set (fromList,union,unions)
import Data.Maybe
import qualified Data.Map as Map

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
data POper  = POper1 | POperN

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

children :: PPTree a -> [PPTree a]
children (Leaf x n) = []
children (Silent n) = []
children (Node1 op u r n) = [u]
children (NodeN op ptl n) = ptl


-- Constructors for NodeN auto-consolidate
pptree :: POperN -> [PPTree a] -> Weight -> PPTree a
pptree poper (u1:u2:ptl) w  = NodeN poper (u1:u2:ptl) w
pptree poper [u1] w         = u1
pptree poper []  w          = emptyTree

pptreeSort :: (Ord a) => POperN -> [PPTree a] -> Weight -> PPTree a
pptreeSort poper ptl w = pptree poper (sort ptl) w

seqP :: [PPTree a] -> Weight -> PPTree a
seqP = pptree Seq 

choiceP :: (Ord a) => [PPTree a] -> Weight -> PPTree a
choiceP = pptreeSort Choice

concP :: (Ord a) => [PPTree a] -> Weight -> PPTree a
concP = pptreeSort Conc


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

scale :: PPTree a -> Float -> PPTree a
scale (Leaf x w) g = Leaf x (w*g)
scale (Silent w) g = Silent (w*g)
scale (Node1 op x r w) g = Node1 op (scale x g) r (w*g)
scale (NodeN op ptl w) g = NodeN op (map (`scale` g) ptl) (w*g)

seqMerge :: PPTree a -> PPTree a -> PPTree a
seqMerge x y = scale (merge x y) 0.5

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
        ++ concatMap (\pt -> formatPPTreeIndent pt (indent+1) ) ptl  

duplicate string n = concat $ replicate n string


-- Rule types

type PRule a = PPTree a -> PPTree a
type LRule a = [PPTree a] -> [PPTree a]

data TRule a = TRule{ rulename :: String, trule :: PRule a }

type PPTRuleTransform a = PPTree a -> [TRule a] -> PPTree a


-- Rules

silentSeq :: PRule a
silentSeq (NodeN Seq ((Silent _):pts) w) = silentSeq (NodeN Seq pts w)
silentSeq (NodeN Seq (pt:pts) w)         = NodeN Seq (pt:ptsr) w
            where NodeN Seq ptsr w2 = silentSeq (NodeN Seq pts w)
silentSeq x = x

silentConc :: PRule a
silentConc (NodeN Conc ((Silent _):pts) w) = silentConc (NodeN Conc pts w)
silentConc (NodeN Conc (pt:pts) w)         = NodeN Conc (pt:ptsr) w
            where NodeN Conc ptsr w2 = silentConc (NodeN Conc pts w)
silentConc x = x

singleNodeOp :: PRule a
singleNodeOp (NodeN op [u] w)  = u
singleNodeOp x = x

choiceChildMR :: (Eq a, Ord a) => 
    ([PPTree a] -> [PPTree a]) -> PPTree a -> PPTree a
choiceChildMR crule (NodeN Choice ptl w) 
    | ptl /= cr  = choiceP cr w
    where cr = crule ptl
choiceChildMR crule x = x

choiceSim :: (Eq a, Ord a) => PRule a
choiceSim = choiceChildMR choiceSimList

choiceSimList :: (Eq a, Ord a) => LRule a
choiceSimList (u1:u2:ptl) | u1 =~= u2 = choiceSimList (merge u1 u2:ptl)
                          | otherwise = u1 : choiceSimList (u2:ptl)
choiceSimList x = x

concSim :: Eq a => PRule a
concSim (NodeN Conc ptl w)
    | ptl /= cr = NodeN Conc cr w
    where cr = concSimList ptl
concSim x = x

concSimList :: (Eq a) => LRule a
concSimList (u1:u2:ptl) 
    | u1 =~= u2 = concSimList (Node1 FLoop (merge u1 u2) 2 (w1+w2):ptl)
    | otherwise = u1:concSimList (u2:ptl)
        where w1 = weight u1 
              w2 = weight u2
concSimList x = x


fixedLoopRoll :: (Eq a, Ord a) => PRule a
fixedLoopRoll (NodeN Seq ptl w) 
    | nptl /= ptl  = seqP nptl w
    where (lss, _) = length ptl `divMod` 2
          rs       = sortOn ncountL (fixedLoopRollForN ptl lss)
          nptl     = head rs
fixedLoopRoll x = x    

fixedLoopRollList :: (Eq a) => [PPTree a] -> PPTree a -> Float -> [PPTree a]
fixedLoopRollList ((Node1 FLoop u1 r1 w1):ptl) prev ct 
    | u1 == prev = fixedLoopRollList ptl prev (ct+r1-1)
    | u1 /= prev = fixedLoopRollEndPattern prev ct:
                        fixedLoopRollList ptl u1 r1
fixedLoopRollList (u1:ptl) prev ct 
    | u1 == prev = fixedLoopRollList ptl prev (ct+1)
    | u1 /= prev = fixedLoopRollEndPattern prev ct:
                            fixedLoopRollList ptl u1 1
fixedLoopRollList [] prev ct = [fixedLoopRollEndPattern prev ct]

loopRollEndPattern :: (Eq a) => PPTree a -> Float -> POper1 -> PPTree a
loopRollEndPattern prev ct poper
    | ct > 1  = Node1 poper prev ct (weight prev)
    | ct <= 1 = prev

fixedLoopRollEndPattern :: (Eq a) => PPTree a -> Float -> PPTree a
fixedLoopRollEndPattern prev ct = loopRollEndPattern prev ct FLoop 

ncountL :: [PPTree a] -> Int
ncountL ptl = sum $ map ncount ptl

fixedLoopRollForN :: (Eq a) => [PPTree a] -> Int -> [[PPTree a]]
fixedLoopRollForN iptl ls 
    | ls >= 1 && length ptl >= ls 
            = [fixedLoopRollListN ptl ss 1]
                ++ map ([pth] ++) (fixedLoopRollForN (tail iptl) ls) 
                ++ fixedLoopRollForN iptl (ls-1) 
                ++ [existingLoopRoll iptl]
    | ls >= 1 && length ptl < ls = [iptl]
    | ls == 0 = []
    where (ss, ptl) = splitAt ls iptl
          pth = head iptl

-- length == 1
existingLoopRoll :: (Eq a) => [PPTree a] -> [PPTree a]
existingLoopRoll ((Node1 FLoop u1 r1 w1):(Node1 FLoop u2 r2 w2):ptl)
    | u1 == u2 = existingLoopRoll (Node1 FLoop u1 (r1+r2) w1:ptl)
existingLoopRoll ((Node1 FLoop u1 r1 w1):u2:ptl)
    | u1 == u2 = existingLoopRoll (Node1 FLoop u1 (r1+1) w1 :ptl)
existingLoopRoll (u1:(Node1 FLoop u2 r2 w2):ptl)
    | u1 == u2 = existingLoopRoll (Node1 FLoop u1 (r2+1) w2: ptl)
existingLoopRoll (u1:ptl) = u1:existingLoopRoll ptl
existingLoopRoll []       = []

fixedLoopRollListN :: (Eq a) => [PPTree a] -> [PPTree a] -> Float -> [PPTree a]
fixedLoopRollListN iptl prev ct 
    | length iptl < length prev 
        = fixedLoopRollEndPatternL prev ct ++ iptl
    | length iptl >= length prev && next == prev 
        = fixedLoopRollListN ptl prev (ct+1)
    | length iptl >= length prev && next /= prev 
        = fixedLoopRollEndPatternL prev ct ++
                            fixedLoopRollListN ptl next 1
    where (next, ptl) = splitAt (length prev) iptl



loopRollEndPatternL :: (Eq a) => [PPTree a] -> Float -> POper1 -> [PPTree a]
loopRollEndPatternL prev ct poper
    | ct > 1  = [Node1 poper (seqP prev w) ct w]
    | ct <= 1 = prev
    where w = weight $ head prev

fixedLoopRollEndPatternL :: (Eq a) => [PPTree a] -> Float -> [PPTree a]
fixedLoopRollEndPatternL prev ct = loopRollEndPatternL prev ct FLoop 

-- no loops of subseq >= 2
probLoopRoll :: Eq a => PRule a
probLoopRoll (NodeN Seq (u1:ptl) w) 
    | nptl /= ptl = NodeN Seq nptl w
    where nptl = probLoopRollList ptl u1 1
probLoopRoll x = x    

probLoopRollList :: (Eq a) => [PPTree a] -> PPTree a -> Float -> [PPTree a]
probLoopRollList ((Node1 PLoop u1 r1 w1):ptl) prev ct
    | u1 =~= prev = probLoopRollList ptl (seqMerge u1 prev) (ct+r1-1)
    | not(u1 =~= prev) = probLoopRollEndPattern prev ct:
                           probLoopRollList ptl u1 r1
probLoopRollList (u1:ptl) prev ct 
    | u1 =~= prev            = probLoopRollList ptl (seqMerge u1 prev) (ct+1)
    | not (u1 =~= prev) = probLoopRollEndPattern prev ct:
                            probLoopRollList ptl u1 1
probLoopRollList [] prev ct = [probLoopRollEndPattern prev ct]
     

probLoopRollEndPattern :: (Eq a) => PPTree a -> Float -> PPTree a
probLoopRollEndPattern prev ct = loopRollEndPattern prev ct PLoop


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

loopGeo :: (Eq a, Ord a) => PRule a
loopGeo = choiceChildMR loopGeoList

loopGeoList :: (Eq a) => LRule a
loopGeoList ((Node1 FLoop u1 r1 w1):(Node1 FLoop u2 r2 w2):ptl) 
    | u1 =~= u2 = loopGeoList  (
                    Node1 PLoop (merge u1 u2) 
                                 (((r1*w1)+(r2*w2))/(w1+w2)) 
                                 (w1+w2) 
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

-- choice folds

seqPrefixMerge :: (Eq a) => [PPTree a] -> [PPTree a]
seqPrefixMerge ((NodeN Seq (pt1:ptl1) w1):(NodeN Seq (pt2:ptl2) w2):ptl)
    | pt1 =~= pt2 && (ptl1 /= [] || ptl2 /= [])
          = NodeN Seq [merge pt1 pt2,
                       NodeN Choice [NodeN Seq ptl1 w1,
                                      NodeN Seq ptl2 w2] nw] nw:
                     seqPrefixMerge ptl
    | pt1 =~= pt2 && (null ptl1 && null ptl2) 
         = merge pt1 pt2:seqPrefixMerge ptl
    | otherwise = NodeN Seq (pt1:ptl1) w1:
                    seqPrefixMerge (NodeN Seq (pt2:ptl2) w2:ptl)
    where nw = w1+w2
seqPrefixMerge ptl = ptl

choiceFoldPrefix :: (Eq a, Ord a) => PRule a
choiceFoldPrefix = choiceChildMR seqPrefixMerge 

-- Warning last is O(N) on lists
seqSuffixMerge :: (Eq a) => [PPTree a] -> [PPTree a]
seqSuffixMerge ((NodeN Seq ptl1 w1):(NodeN Seq ptl2 w2):ptl)
    | pt1 =~= pt2 = NodeN Seq [NodeN Choice [NodeN Seq nptl1 w1,
                                                NodeN Seq nptl2 w2] nw,
                                 merge pt1 pt2] nw:
                     seqSuffixMerge ptl
    | otherwise = NodeN Seq ptl1 w1:
                  seqSuffixMerge (NodeN Seq ptl2 w2:ptl)
     where pt1 = last ptl1
           pt2 = last ptl2
           nptl1 = take (length ptl1-1) ptl1
           nptl2 = take (length ptl2-1) ptl2
           nw = w1+w2
seqSuffixMerge ptl = ptl

-- duplication across prefix suffix folds and maybe other choice
choiceFoldSuffix :: (Eq a, Ord a) => PRule a
choiceFoldSuffix = choiceChildMR seqSuffixMerge


-- conc creation
concFromChoice :: (Eq a, Ord a) => PRule a
concFromChoice = choiceChildMR concFromChoiceList 

isNontrivSeq :: PPTree a -> Bool
isNontrivSeq (NodeN Seq pt w) = length pt > 1
isNontrivSeq x                = False

concFromChoiceList :: (Eq a, Ord a) => LRule a
concFromChoiceList ptl
    | length rs /= length sq  = rs ++ fl
    | otherwise               = ptl
    where sq = filter isNontrivSeq ptl
          fl = filter (not . isNontrivSeq) ptl
          rs = concFromSeqList sq

concFromSeqList :: (Ord a) => [PPTree a] -> [PPTree a]
concFromSeqList ptl 
    | length rs /= length hds 
        = map (\(x,y) -> convertConcMapEntryToNode x y) (Map.toList rs)
    | otherwise               = ptl
    where (hds,tls) = unzip $ map (\x -> splitAt 2 (children x)) ptl
          rs   = concMapFromSeqChildren hds tls

convertConcMapEntryToNode :: (Ord a) => [PPTree a] -> [[PPTree a]] -> PPTree a
convertConcMapEntryToNode ptl tptl
    | length tptl > 1  && length cc >= 1 = seqP [concP ptl w, choiceP cc w] w
    | length tptl > 1  && length cc == 0 = concP ptl w
    | otherwise        = seqP (ptl ++ (head tptl) ) w
    where w  = sum $ map weight ptl
          cc = mapMaybe convertConcTailEntry tptl

convertConcTailEntry :: [PPTree a] -> Maybe (PPTree a)
convertConcTailEntry [pt] = Just pt
convertConcTailEntry []   = Nothing
convertConcTailEntry ptl  = Just (NodeN Seq ptl (weight $ head ptl))

-- pre: sublists of len 2
concMapFromSeqChildren :: (Ord a) => [[PPTree a]] -> [[PPTree a]]
                                        -> Map.Map [PPTree a] [[PPTree a]]
concMapFromSeqChildren (ptl:ptls) (tptl:tptls)
    | ptl `Map.member` sm = Map.update (\x -> Just (tptl:x) ) ptl sm
    | pml `Map.member` sm = Map.update (\x -> Just (tptl:x) ) pml sm
    | otherwise            = Map.insert ptl [tptl] sm
    where sm  = concMapFromSeqChildren ptls tptls
          pml = reverse ptl
concMapFromSeqChildren [] _ = Map.empty


-- Rule lists

baseRuleList :: (Eq a, Ord a) => [TRule a]
baseRuleList = [
            TRule{rulename="singleNodeOp",trule=singleNodeOp},
            TRule{rulename="flatten",trule=flatten},
            TRule{rulename="fixedLoopRoll", trule=fixedLoopRoll},
            TRule{rulename="silentSeq",trule=silentSeq},
            TRule{rulename="silentConc",trule=silentSeq},
            TRule{rulename="choiceSim",trule=choiceSim},
            TRule{rulename="concSim",trule=concSim},
            TRule{rulename="choiceFoldPrefix",trule=choiceFoldPrefix},
            TRule{rulename="choiceFoldSuffix",trule=choiceFoldSuffix},
            TRule{rulename="loopNest",trule=loopNest},
            TRule{rulename="loopGeo",trule=loopGeo},
            TRule{rulename="concFromChoice",trule=concFromChoice},
            TRule{rulename="loopFixToProb", trule=loopFixToProb},
            TRule{rulename="probLoopRoll", trule=loopFixToProb}
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
-- vrule x r = trule r x
-- validation disabled
vrule x r = validateDebug x y r y
     where y = trule r x

validateDebug :: (Eq a, Show a) => PPTree a -> PPTree a -> TRule r -> b -> b
validateDebug x y r val
    | x /= y && validate y       = debug msg val
    | x /= y && not (validate y) =
        debug ("*** invalid tree *** "
                ++ valMsg (verboseValidate y)
                ++ " :: " ++ msg) val
    | x == y = val
    -- | x == y = debug (rulename r ++ " 000 " ++ show x) val
    -- super-verbose option
    where msg =  rulename r ++ " " ++ show x ++ " => " ++ show y


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



validate :: PPTree a -> Bool
validate (NodeN Seq ptl w)
    = w == head sw && length sw == 1 && validateList ptl
    where sw = nub $ map weight ptl
validate (NodeN Choice ptl w) = w == sum (map weight ptl) && validateList ptl
validate (NodeN Conc ptl w)   = w == sum (map weight ptl) && validateList ptl
validate (Node1 op x m w)     = w == weight x && validate x
validate x = True

validateList :: [PPTree a] -> Bool
validateList = all validate 

data Validation = Validation{valResult::Bool, valMsg:: String}
    deriving (Show,Eq)
valOk = Validation{valResult=True, valMsg="Ok"}
verboseValidate :: (Show a) => PPTree a -> Validation
verboseValidate (NodeN Seq ptl w)
    | validate  (NodeN Seq ptl w)  = valOk
    | length sw /= 1 || w /= head sw
                    = Validation{valResult=False ,
                                valMsg="Seq " ++ show w ++ " /= "
                                    ++ show sw ++ " in " ++ sn }
    | not (validateList ptl) = verboseValidateList ptl
    where sn =  show (NodeN Seq ptl w)
          sw = nub $ map weight ptl
verboseValidate (NodeN Choice ptl w)
    | validate (NodeN Choice ptl w) = valOk
    | w /= ws = Validation{valResult=False,
                    valMsg="Choice " ++ show w ++ " /= "
                        ++ show ws ++ " in " ++ sn}
    | not(validateList ptl) = verboseValidateList ptl
    where sn =  show (NodeN Choice ptl w)
          ws = sum $ map weight ptl
verboseValidate (NodeN Conc ptl w)
    | validate (NodeN Conc ptl w) = valOk
    | w /= ws = Validation{valResult=False,
                    valMsg="Conc " ++ show w ++ " /= "
                        ++ show ws ++ " in " ++ sn}
    | not(validateList ptl) = verboseValidateList ptl
    where sn =  show (NodeN Conc ptl w)
          ws = sum $ map weight ptl
verboseValidate (Node1 op x m w)
    | w /= weight x = Validation{valResult=False,
                valMsg="Node1 " ++ show op ++ " " ++ show w ++ " /= "
                ++ show (weight x)  }
    | not(validate x) = verboseValidate x
    where sn = show (Node1 op x m w)
verboseValidate x  = valOk

verboseValidateList :: (Show a) => [PPTree a] -> Validation
verboseValidateList (pt:ptl) 
    | validate pt       = verboseValidateList ptl
    | not (validate pt) = Validation{
                              valResult=False, 
                              valMsg=valMsg (verboseValidate pt)
                                  ++ valMsg (verboseValidateList ptl) }
verboseValidateList []  = valOk


