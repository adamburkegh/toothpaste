module Binpaste where

import PetriNet
import EventLog
import Data.List (sort)
import Data.Set (Set,toList,fromList,union)
import Debug.Trace

-- Some data structures and manipulations for process trees and 
-- similar structures

debug :: String -> a -> a
-- Debug ON
debug = trace

-- Debug OFF
-- debug x y = y 

changeDebug :: Bool -> String -> a -> a
changeDebug cond msg x = if cond then debug ("=> " ++ msg) x else x

-- Process Trees

type Repeat = Float

data PPTree a = Leaf a Weight
               | Silent Weight
               | Node1 POper1 (PPTree a) Repeat Weight 
               | Node2 POper2 (PPTree a) (PPTree a) Weight
                deriving (Show,Ord)

data POper2 = Choice | Seq | Conc deriving (Enum,Show,Eq,Ord)
data POper1 = PLoop | FLoop deriving (Enum,Show,Eq,Ord)
data POper = POper1 | POper2

silent :: Weight -> PPTree String
silent = Silent 

-- Eq may not be very useful though
instance (Eq a) => Eq (PPTree a) where
    Leaf a n == Leaf b m = a == b && n == m
    Silent n == Silent m = n == m
    Node1 p1 a r1 n == Node1 p2 b r2 m 
        = (p1 == p2) && (a == b) && (r1 == r2) && (n == m)
    Node2 po1 pt1a pt1b n == Node2 po2 pt2a pt2b m 
        = (po1 == po2) && (pt1a == pt2a) && (pt1b == pt2b) && (n == m)
    x == y = False

-- Structural equality aka similarity
-- No hash for labels so may be expensive in this impl, esp for large trees
(=~=) :: (Eq a) => PPTree a -> PPTree a -> Bool
(Leaf a n) =~= (Leaf b m)   = a == b
(Silent n) =~= (Silent m)   = True
(Node1 op1 x r1 w1) =~= (Node1 op2 y r2 w2)  
    | op1 == PLoop = op1 == op2 && x =~= y
    | op1 == FLoop = op1 == op2 && r1 == r2 && x =~= y
(Node2 op1 x1 y1 w1) =~= (Node2 op2 x2 y2 w2)
    = op1 == op2 && x1 =~= x2 && y1 =~= y2
x =~= y = False

type PPTRuleTransform a = PPTree a -> [TRule a] -> PPTree a 

weight :: PPTree a -> Weight
weight (Leaf x n) = n
weight (Silent n) = n
weight (Node1 op x r n) = n
weight (Node2 op x y n) = n

-- Careful using this one - it breaks consistency and can produce invalid trees
-- Currently only used in converting a loop to a PetriNet - not from rules
replaceWeight :: PPTree a -> Weight -> PPTree a
replaceWeight (Leaf x n) w = Leaf x w
replaceWeight (Silent n) w = Silent w
replaceWeight (Node1 op x r n) w = Node1 op x r w
replaceWeight (Node2 op x y n) w = Node2 op x y w

formatPPTree :: (Show a) => PPTree a -> String
formatPPTree x = formatPPTreeIndent x 0

indentStr = "  "

-- tau = "\120591"
tau = "tau" -- encoding issues for Haskell on windows, TODO

formatWeight :: Weight -> String
formatWeight n = ":" ++ show n ++ "\n"

formatPPTreeIndent :: (Show a) => PPTree a -> Int -> String
formatPPTreeIndent (Leaf x n) indent = 
    duplicate indentStr indent ++ show x ++ formatWeight n 
formatPPTreeIndent (Silent n) indent = 
    duplicate indentStr indent ++ tau ++ formatWeight n 
formatPPTreeIndent (Node1 op x r n) indent = 
    duplicate indentStr indent ++ show op ++ "[" ++ show r ++ "]"
        ++ formatWeight n 
        ++ formatPPTreeIndent x (indent+1)
formatPPTreeIndent (Node2 op x y n) indent = 
    duplicate indentStr indent ++ show op  ++ formatWeight n 
        ++ formatPPTreeIndent x (indent+1)
        ++ formatPPTreeIndent y (indent+1)


duplicate string n = concat $ replicate n string

putPPTree :: (Show a) => PPTree a -> IO ()
putPPTree = putStrLn . formatPPTree 

emptyTree :: PPTree a
emptyTree = Silent 0




-- pre: input trees are structurally equal
-- note implementation always takes lhs for efficiency
merge :: PPTree a -> PPTree a -> PPTree a
merge (Leaf x w1) (Leaf y w2) = Leaf x (w1+w2)
merge (Silent w1) (Silent w2) = Silent (w1+w2)
merge (Node1 FLoop x r1 w1) (Node1 op2 y r2 w2) 
   = Node1 FLoop (merge x y) r1 (w1+w2)
merge (Node1 PLoop x r1 w1) (Node1 op2 y r2 w2) 
   = Node1 PLoop (merge x y) ((w1*r1+w2*r2)/(w1+w2)) (w1+w2)
merge (Node2 op1 x1 y1 w1) (Node2 op2 x2 y2 w2) 
    = Node2 op1 (merge x1 x2) (merge y1 y2) (w1+w2)

epsilon = 0.0001

scale :: PPTree a -> Float -> PPTree a
scale (Leaf x w) g = Leaf x (w*g)
scale (Silent w) g = Silent (w*g)
scale (Node1 op x r w) g = Node1 op (scale x g) r (w*g)
scale (Node2 op x y w) g = Node2 op (scale x g) (scale y g) (w*g)

weightForce :: PPTree a -> Weight -> PPTree a
weightForce (Leaf x w) g = Leaf x g
weightForce (Silent w) g = Silent g
weightForce (Node1 op x r w) g = Node1 op x r g
weightForce (Node2 op x y w) g = Node2 op x y g

-- reduce compounding floating point errors by forcing to known weight
-- when within an error epsilon
scaleForce :: PPTree a -> Float -> Float -> PPTree a
scaleForce u g wref 
    | abs(w1*g - wref) < epsilon  = weightForce su wref
    | abs(w1*g - wref) >= epsilon = su
        where w1 = weight u
              su = scale u g

-- pre: second parameter is a PLoop
loopMean :: PPTree a -> PPTree a -> PPTree a
loopMean u1 (Node1 PLoop x2 r2 w2) 
     = Node1 PLoop (scaleForce (merge u1 (scale x2 r2) ) 
                               ((w1 + w2) / (r2*w2+w1) )
                               (w1+w2) ) 
                  ((w1 + w2*r2)/(w1+w2)) 
                  ( w1 + w2 )
     where w1 = weight u1


choice :: (Ord a) => [PPTree a] -> PPTree a 
choice [] = Silent 0
choice [x] = x
choice (x:xs)  | x <= head xs = choiceP x (choice xs)
               | otherwise    = choiceP (head xs) (choice (x: tail xs) )

choiceP :: (Ord a) => PPTree a -> PPTree a -> PPTree a
choiceP x y 
    | x <= y    = Node2 Choice x y (weight x + weight y)
    | otherwise = Node2 Choice y x (weight x + weight y)

seqP :: PPTree a -> PPTree a -> PPTree a
seqP x y = Node2 Seq x y (weight x)

conc :: (Ord a) => [PPTree a] -> PPTree a
conc [] = Silent 0
conc [x] = x
conc (x:xs) | x <= head xs = concP x (conc xs)
            | otherwise    = concP (head xs) (conc (x: tail xs) )

concP :: (Ord a) => PPTree a -> PPTree a -> PPTree a
concP x y | x <= y    = Node2 Conc x y (weight x + weight y)
          | otherwise = Node2 Conc y x (weight x + weight y)

seqNode :: [PPTree a] -> PPTree a
seqNode [] = Silent 0
seqNode [x] = x
seqNode (x:xs) = Node2 Seq x (seqNode xs) (weight x)

type PRule a = PPTree a -> PPTree a

data TRule a = TRule{ rulename :: String, trule :: PRule a }

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


mapRuleList :: (Show a, Eq a) => PPTree a -> [TRule a] -> PPTree a 
mapRuleList x [] = x
mapRuleList x [r] =  validateDebug x y r
                     y where y = trule r x
mapRuleList x (r:rs) = validateDebug y z r 
                        y where y = trule r z
                                z = mapRuleList x rs

shrunk :: (Eq a) => PPTree a -> PPTree a -> Bool
shrunk pt1 pt2 = pt1 /= pt2 && ncount pt1 < ncount pt2

-- This is the ugliest Haskell code ever written
--
-- It takes an input rule, and returns a commutative version of that rule
-- which is distributed over the operator parameter
-- Eg choice is commutative, so choiceSim is passed to this rule as
--    commutChoice = commutRule Choice choiceSimT
-- This lets binary trees be treated as multi-child trees and so Haskell
-- pattern matching rules only need to be written as two parameter rules,
-- mostly
-- Commutative operators allow sums of component weights
commutRule :: (Eq a) => POper2 -> TRule a -> PPTree a -> PPTree a
commutRule op1 rule (Node2 op2 x (Node2 op3 y z w1) w2) 
    | op1 == op2 && op1 == op3 && rsp /= inp                 = rsp
    | op1 == op2 && op1 == op3 && rsp == inp && rsp2 /= inp2 = rsp2
    | op1 == op2 && op1 == op3 && rsp == inp && rsp2 == inp2 
                               && rsp3 /= inp3 = rsp3
    | op1 == op2 && op1 == op3 && rsp == inp && rsp2 == inp2 
                 && rsp3 == inp3 && shrunk rsr inp = rsr 
    | op1 == op2 && op1 == op3 && rsp == inp && rsp2 == inp2 
                 && rsp3 == inp3 && shrunk rsr2 inp2  = rsr2
    | op1 == op2 && op1 == op3 && rsp == inp && rsp2 == inp2 
                 && rsp3 == inp3 && rsr2 == inp2 
                 && shrunk rsr3 inp3 = rsr3 
      where inp  = Node2 op2 x (Node2 op3 y z w1) w2
            inp2 = Node2 op2 (Node2 op2 x y wxy) z w2
            inp3 = Node2 op2 (Node2 op2 x z wxz) y w2
            rsp  = trule rule inp
            rsp2 = trule rule inp2
            rsp3 = trule rule inp3
            rsr  = Node2 op2 x (commutRule op1 rule (Node2 op3 y z w1) ) w2
            rsr2 = Node2 op2 (commutRule op1 rule (Node2 op2 x y wxy)) z w2
            rsr3 = Node2 op2 (commutRule op1 rule (Node2 op2 x z wxz)) y w2
            wxy  = weight x + weight y
            wxz  = weight x + weight z
commutRule op1 rule (Node2 op2 (Node2 op3 x y w1) z w2) 
    | op1 == op2 && op1 == op3 && rs /= inp = rs
      where inp = Node2 op1 x (Node2 op1 y z (weight y + weight z) ) w2
            rs  = commutRule op1 rule inp
commutRule op rule x = trule rule x

commutTRule :: (Eq a) => POper2 -> (PPTree a -> PPTree a) -> String -> TRule a
commutTRule oper rule rname = TRule{rulename="commute " ++ rname, 
                            trule=commutRule oper TRule{rulename="rname", 
                                                        trule=rule }}




choiceSim :: (Eq a) => PRule a
choiceSim (Node2 Choice x y n) 
             | x =~= y = merge x y
choiceSim x = x 

-- would be good to have a more general way to do this, like in the paper
loopChoiceSim :: (Eq a) => PRule a
loopChoiceSim (Node2 Choice (Node1 PLoop x r w1) y w2) 
             | x =~= y = loopMean y (Node1 PLoop x r w1)
loopChoiceSim (Node2 Choice y (Node1 PLoop x r w1) w2) 
             | x =~= y = loopMean y (Node1 PLoop x r w1)
loopChoiceSim x = x 

choiceFold :: (Eq a, Ord a) => PRule a
choiceFold (Node2 Choice 
                (Node2 Seq x1 y1 w1) 
                (Node2 Seq x2 y2 w2) w3) 
                | x1 =~= x2 = Node2 Seq (merge x1 x2) (choiceP y1 y2) w3
                | y1 =~= y2 = Node2 Seq (choiceP x1 x2) (merge y1 y2) w3
                | z1 =~= z2 = Node2 Seq (choiceP h1 h2) (merge z1 z2) w3
                    where (Node2 Seq h1 z1 w4) = seqSuffix (Node2 Seq x1 y1 w1) 
                          (Node2 Seq h2 z2 w5) = seqSuffix (Node2 Seq x2 y2 w2)
choiceFold x = x


loopChoiceFold :: (Eq a, Ord a) => PRule a
loopChoiceFold (Node2 Choice (Node2 Seq x1 y1 w1) 
                             (Node2 Seq (Node1 PLoop x2 r2 w2) 
                                         y2 w3) w4) 
                | x1 =~= x2 = Node2 Seq (loopMean x1 lx)
                                        (choiceP y1 y2) w4
                      where lx = Node1 PLoop x2 r2 w2
loopChoiceFold (Node2 Choice (Node2 Seq (Node1 PLoop x1 r1 w1) 
                                        y1 w2) 
                             (Node2 Seq x2 y2 w3) w4) 
               | x1 =~= x2 = Node2 Seq (loopMean x2 lx)
                                       (choiceP y1 y2) w4
                      where lx = Node1 PLoop x1 r1 w1
loopChoiceFold x = x


choiceSkip :: (Eq a, Ord a) => PRule a
choiceSkip (Node2 Choice x
                        (Node2 Seq y z w1) w2)  
                  | x =~= y    = seqP (merge x y) 
                                      (choiceP z silentx)
                  | x =~= z    = seqP (choiceP y silentx) 
                                      (merge x z)
                  | x =~= z1   = seqP (choiceP h1 silentx) 
                                      (merge x z1)
                   where (Node2 Seq h1 z1 wz1) = seqSuffix (Node2 Seq y z w1) 
                         silentx               = Silent (weight x)
choiceSkip (Node2 Choice (Node2 Seq x y w1) z w2) 
                  | x  =~= z   = seqP (merge x z) 
                                      (choiceP y silentz)
                  | y  =~= z   = seqP (choiceP x silentz)
                                      (merge y z) 
                  | y1 =~= z   = seqP (choiceP h1 silentz) 
                                      (merge z y1)
                     where (Node2 Seq h1 y1 wy1) = seqSuffix (Node2 Seq x y w1) 
                           silentz               = Silent (weight z)
choiceSkip x = x

fixedLoopRoll :: Eq a => PRule a
fixedLoopRoll (Node2 Seq x y w) | x == y = Node1 FLoop x 2 w
fixedLoopRoll (Node2 Seq x (Node2 Seq y z w1) w2) 
    | x == y = Node2 Seq (Node1 FLoop x 2 w2) z w2
fixedLoopRoll (Node2 Seq x (Node1 FLoop y r w1) w2) 
    | x == y = Node1 FLoop x (r+1) w2
fixedLoopRoll (Node2 Seq x (Node2 Seq (Node1 FLoop y r w1) z w2) w3) 
    | x == y = Node2 Seq (Node1 FLoop x (r+1) w3) z w3
fixedLoopRoll (Node2 Seq (Node1 FLoop x r w1) y w2) 
    | x == y = Node1 FLoop x (r+1) w2
fixedLoopRoll (Node2 Seq (Node1 FLoop x rx wx) (Node2 Seq y z ws) w) 
    | x == y = Node2 Seq (Node1 FLoop x (rx+1) wx) z w
fixedLoopRoll x = x

seqMerge :: PPTree a -> PPTree a -> PPTree a
seqMerge x y = scale (merge x y) 0.5

probLoopRoll :: Eq a => PRule a
probLoopRoll (Node2 Seq x y w) | x =~= y = Node1 PLoop x 2 w
probLoopRoll (Node2 Seq x (Node2 Seq y z w1) w2) 
    | x =~= y = Node2 Seq (Node1 PLoop (seqMerge x y) 2 w2) z w2
probLoopRoll (Node2 Seq x (Node1 PLoop y r w1) w2) 
    | x =~= y = Node1 PLoop (seqMerge x y) (r+1) w2
probLoopRoll (Node2 Seq x (Node2 Seq (Node1 PLoop y r w1) z w2) w3) 
    | x =~= y = Node2 Seq (Node1 PLoop (seqMerge x y) (r+1) w3) z w3
probLoopRoll (Node2 Seq (Node1 PLoop x r w1) y w2) 
    | x =~= y = Node1 PLoop (seqMerge x y) (r+1) w2
probLoopRoll (Node2 Seq (Node1 PLoop x rx wx) (Node2 Seq y z ws) w) 
    | x =~= y = Node2 Seq (Node1 PLoop (seqMerge x y) (rx+1) wx) z w
probLoopRoll x = x


loopNest :: PRule a
loopNest (Node1 FLoop (Node1 FLoop x r1 w1) r2 w2) = Node1 FLoop x (r1*r2) w2
loopNest (Node1 FLoop (Node1 PLoop x rp wp) rf wf) 
    | rf > 1 = Node1 PLoop x (rp*(rf-1)) wf
loopNest (Node1 PLoop (Node1 op x rf wf) rp wp)  
    | op == FLoop || op == PLoop = Node1 PLoop x (rf*rp) wp
loopNest x = x

loopGeo :: (Eq a) => PRule a
loopGeo (Node2 Choice (Node1 FLoop x1 r1 w1) (Node1 FLoop x2 r2 w2) w3) 
    | x1 =~= x2 = Node1 PLoop (merge x1 x2) (((r1*w1)+(r2*w2))/(w1+w2)) (w1+w2)
loopGeo x = x

loopFixToProb :: PRule a
loopFixToProb (Node1 FLoop x m w) = Node1 PLoop x m w
loopFixToProb x = x

-- assoc 
normSeq :: (Eq a) => PRule a 
normSeq (Node2 Seq (Node2 Seq x y w1) z w2) = 
    Node2 Seq (normSeq x) (normSeq (Node2 Seq y z w2) ) w2
normSeq (Node2 Seq x y w)  = Node2 Seq (normSeq x) (normSeq y) w
normSeq x = x

longSeq :: (Eq a) => PPTree a -> PPTree a
longSeq (Node2 Seq x y w) = lrs $ normSeq (Node2 Seq x y w)
longSeq x = x

-- pre: both normalized
prefix :: (Eq a) => PPTree a -> PPTree a -> Bool
prefix (Node2 Seq x1 y1 w1) (Node2 Seq x2 y2 w2) 
    | x1 == x2 = prefix y1 y2
    | x1 /= x2 = False
prefix x (Node2 Seq y z w) = x == y
prefix pt1 pt2             = pt1 == pt2

-- pre: start is normalized sequence form as per normSeq
-- Isn't actually longest repeated substring
lrs :: (Eq a) => PPTree a -> PPTree a 
lrs (Node2 Seq x (Node2 Seq y z w1) w2) 
    | prefix x y && not (cuty =~= Silent 1 )
        = Node2 Seq (Node1 FLoop x 2 w2) (Node2 Seq cuty z w2) w2
    | prefix x y && cuty =~= Silent 1 = Node2 Seq (Node1 FLoop x 2 w2) z w2 
    | prefix newsub z && not (newsuf =~= Silent 1) 
                = Node2 Seq (Node1 FLoop newsub 2 w2) newsuf w2
    | prefix newsub z && newsuf =~= Silent 1
                = Node1 FLoop newsub 2 w2
    where newsub  = Node2 Seq x y w2
          cuty   = rmpref x y
          newsuf = rmpref newsub z
lrs (Node2 Seq x y w) 
    | prefix x y && not (newsuf =~= Silent 1 ) 
        = Node2 Seq (Node1 FLoop x 2 w) newsuf w
    | prefix x y && newsuf =~= Silent 1 = Node1 FLoop x 2 w
                        where newsuf = rmpref x y
lrs x = x

-- rmpref (prefix,ptree)
rmpref :: (Eq a) => PPTree a -> PPTree a -> PPTree a 
rmpref (Node2 Seq x1 y1 w1) (Node2 Seq x2 y2 w2) 
    | x1 == x2 && rs /= y2  = rs
    | x1 /= x2              = Node2 Seq x2 y2 w2
    where rs = rmpref y1 y2
rmpref x (Node2 Seq y z w) | x == y = rmpref x z
rmpref pt1 pt2             | pt1 == pt2 = Silent (weight pt1)
rmpref pr x = x

---
--
-- would be good to have a more general way to do this with loop similarity, 
-- like in the paper
choiceRoll :: (Eq a, Ord a) => PPTree a -> PPTree a
choiceRoll (Node2 Choice x (Node1 PLoop y r w1) w2) 
                  | x =~= y = loopMean x (Node1 PLoop y r w1)
-- see choiceSkip
choiceRoll (Node2 Choice (Node1 PLoop x r w1) (Node2 Seq y z w2) w3)
                  | x =~= y = Node2 Seq (loopMean y (Node1 PLoop x r w1) ) 
                                        (choiceP z (Silent w1) )  w3
    -- seq assoc?
choiceRoll x = x

concSim :: Eq a => PPTree a -> PPTree a
concSim (Node2 Conc x y w) 
    | x =~= y = Node1 FLoop (merge x y) 2 w
concSim x = x

-- merge preserving weight of first param
concPairMerge :: PPTree a -> PPTree a -> PPTree a
concPairMerge x1 x2 = scale (merge x1 x2) (w/(w+v)) 
    where w = weight x1
          v = weight x2

concFromChoice :: (Eq a, Ord a) => PPTree a -> PPTree a
concFromChoice (Node2 Choice 
                    (Node2 Seq x1 y1 w) 
                    (Node2 Seq y2 x2 v) w3) 
               | x1 =~= x2 && y1 =~= y2 
                    = concP (concPairMerge x1 x2)
                            (concPairMerge y2 y1)
concFromChoice (Node2 Choice 
                    (Node2 Seq x1 (Node2 Seq y1 z1 w1) w2) 
                    (Node2 Seq y2 (Node2 Seq x2 z2 w3) w4) w5) 
               | x1 =~= x2 && y2 =~= y1 
                    = seqP (concP (concPairMerge x1 x2) 
                                  (concPairMerge y2 y1) )
                           (choiceP z1 z2)
concFromChoice (Node2 Choice 
                    (Node2 Seq (Node2 Seq x1 y1 w1) z1 w2) 
                    (Node2 Seq (Node2 Seq y2 x2 w3) z2 w4) w5) 
               | x1 =~= x2 && y1 =~= y2 = seqP (concP (concPairMerge x1 x2) 
                                                      (concPairMerge y2 y1)) 
                                               (choiceP z1 z2)
concFromChoice x = x


concSubsume :: (Eq a, Ord a) => PPTree a -> PPTree a
concSubsume (Node2 Choice (Node2 Seq x1 y1 w1) 
                          (Node2 Conc x2 y2 w2) w3)
    | x1 =~= x2 && y1 =~= y2 = concP (merge x1 x2) (concPairMerge y1 y2)
    | x1 =~= y2 && y1 =~= x2 = concP (concPairMerge x1 y2) (merge y1 x2) 
concSubsume (Node2 Choice (Node2 Conc x1 y1 w1) 
                          (Node2 Seq x2 y2 w2) w3)
    | x1 =~= x2 && y1 =~= y2 = concP (merge x1 x2) (concPairMerge y1 y2)
    | x1 =~= y2 && y1 =~= x2 = concP (concPairMerge x1 y2) (merge y1 x2) 
concSubsume (Node2 Choice (Node2 Seq (Node2 Conc x1 y1 w1) z1 w2) 
                          (Node2 Seq (Node2 Seq x2 y2 w3) z2 w4) w5)
    | x1 =~= x2 && y1 =~= y2 = 
        seqP (concP   (merge x1 x2) (concPairMerge y1 y2) ) 
             (choiceP z1 z2 )
    | x1 =~= y2 && y1 =~= x2 = 
        seqP (concP   (concPairMerge x1 y2) (merge y1 x2) ) 
             (choiceP z1 z2 )
    -- obsolete with seq assoc?
concSubsume (Node2 Choice (Node2 Seq (Node2 Conc x1 y1 w1) z1 w2) 
                          (Node2 Seq x2 (Node2 Seq y2 z2 w3) w4) w5)
    | x1 =~= x2 && y1 =~= y2 = 
        seqP (concP   (merge x1 x2) (concPairMerge y1 y2) ) 
             (choiceP z1 z2 )
    | x1 =~= y2 && y1 =~= x2 = 
        seqP (concP   (merge x1 y2) (concPairMerge y1 x2) ) 
             (choiceP z1 z2 )
    -- obsolete with seq assoc?
concSubsume x = x


silentSeq :: PPTree a -> PPTree a
silentSeq (Node2 Seq x (Silent _) _ ) = x 
silentSeq (Node2 Seq (Silent _) x _) = x 
silentSeq x = x

silentConc :: PPTree a -> PPTree a
silentConc (Node2 Conc x (Silent ws) w2) = scale x (ws+wx/wx) 
    where wx = weight x
silentConc (Node2 Conc (Silent ws) x w2) = scale x (ws+wx/wx) 
    where wx = weight x
silentConc x = x

-- Note these suffix functions expose the suffix as the last item
-- but return the whole sequence 
seqSuffix1 :: [PPTree a] -> PPTree a -> PPTree a
seqSuffix1 head (Node2 Seq x y w) = seqSuffix1 (head++[x]) y
seqSuffix1 head x = Node2 Seq (seqNode head) x (weight x)

seqSuffix :: (Eq a) => PPTree a -> PPTree a
seqSuffix (Node2 Seq x y w) = seqSuffix1 [] (Node2 Seq x y w)
seqSuffix x = x


-- Traverses rules in reverse order at each level of tree
transformWithRules :: (Show a, Eq a) => PPTRuleTransform a
transformWithRules (Node1 op x r w) rules = 
    mapRuleList( Node1 op 
                    (transformWithRules x rules) r w )  
             rules
transformWithRules (Node2 op x y w) rules = 
    mapRuleList (Node2 op
                   (transformWithRules x rules)  
                   (transformWithRules y rules) w )  
            rules
transformWithRules x _ = x

vrule :: (Show a, Eq a) => PPTree a -> TRule a -> PPTree a
vrule x r = validateDebug x y r y
    where y = trule r x

transformPT :: (Show a, Eq a) => PPTree a -> TRule a -> PPTree a 
transformPT (Node1 op x rp w) rl = vrule (Node1 op (transformPT x rl) rp w) rl
transformPT (Node2 op x y w) rl =  
    vrule (Node2 op (transformPT x rl) (transformPT y rl) w) rl
transformPT pt rl = vrule pt rl

-- Exhausts rules in order
transformInRuleOrder :: (Show a, Eq a) => PPTRuleTransform a 
transformInRuleOrder pt [r] = transformPT pt r
transformInRuleOrder pt (r:rs) = 
    transformInRuleOrder (transformInRuleOrder pt [r]) rs
transformInRuleOrder x _ = x

maxTransformBreadth :: (Show a, Eq a) => PPTRuleTransform a
maxTransformBreadth x rules | x == y      = x
                     | otherwise   = maxTransformBreadth y rules
                     where y = debug ("=== Count:" ++ show (ncount x) ++  
                                  "===") (transformWithRules x (reverse rules) )

maxTransformRuleOrder :: (Show a, Eq a) => PPTRuleTransform a
maxTransformRuleOrder x rules | x == y      = x
                     | otherwise   = maxTransformRuleOrder y rules
                     where y = debug ("=== Count:" ++ show (ncount x) 
                                    ++  "===") (transformInRuleOrder x rules)


baseRuleList :: (Eq a, Ord a) => [TRule a]
baseRuleList = [commutTRule Choice choiceSim "choiceSim",
            commutTRule Choice choiceFold "choiceFold", 
            commutTRule Choice loopChoiceFold "loopChoiceFold", 
            commutTRule Choice choiceSkip "choiceSkip",
            commutTRule Choice choiceRoll "choiceRoll" , 
            commutTRule Conc concSim "concSim", 
            commutTRule Choice concSubsume "concSubsume",
            commutTRule Choice concFromChoice "concFromChoice", 
            TRule{rulename="fixedLoopRoll",trule=fixedLoopRoll}, 
            TRule{rulename="probLoopRoll",trule=probLoopRoll}, 
            TRule{rulename="longSeq",trule=longSeq} , 
            TRule{rulename="loopNest",trule=loopNest}, 
            commutTRule Choice loopGeo "loopGeo",
            TRule{rulename="silentSeq",trule=silentSeq},
            commutTRule Conc silentConc "silentConc", 
            TRule{rulename="loopFixToProb", trule=loopFixToProb}] 

ruleList :: (Show a, Eq a, Ord a) => [TRule a]
ruleList = baseRuleList


transform :: (Show a, Eq a, Ord a) => PPTree a -> PPTree a
transform = transformClean

transformClean x = maxTransformBreadth x ruleList 
transformRuleOrdered x = maxTransformRuleOrder x ruleList 

ncount :: PPTree a -> Int
ncount (Leaf a _) = 1
ncount (Silent _) = 1
ncount (Node1 p a _ _) = 1 + ncount a 
ncount (Node2 p a b _)  = 1 + ncount a  + ncount b 


-- mining

traceModel :: (Ord a, Eq a) => Log a -> PPTree a
traceModel = traceModelR 1 . sort 


traceModelR :: (Eq a) => Weight -> Log a -> PPTree a
traceModelR rf [] = emptyTree
traceModelR rf [x] = tracePPTree rf x
-- trace order version: inferior, but showed a rule bug
-- traceModel (x:xs) = Node2 Choice (tracePPTree x) (traceModel xs)
-- lexical order version - better, but concealed a bug 
-- in association rules
traceModelR rf (x:y:xs) 
           | x == y  = traceModelR (rf+1) (x:xs) 
           | x /= y  = Node2 Choice u1 u2 (rf+weight u2)
            where u1 = tracePPTree rf x 
                  u2 = traceModelR 1 (y:xs)

tracePPTree :: Weight -> Trace a -> PPTree a
tracePPTree rf [x] = Leaf x rf
tracePPTree rf (x:xs) = Node2 Seq (Leaf x rf) (tracePPTree rf xs) rf

discover :: Parser -> String -> PPTree String
discover parser = transform . traceModel . parser

discoverGen :: (Ord a, Eq a, Show a) => Log a -> PPTree a
discoverGen log = transform $ traceModel log

batchIncDiscover :: (Ord a, Eq a, Show a) => Log a -> PPTree a
batchIncDiscover []     = emptyTree
batchIncDiscover [t]    = tracePPTree 1 t
batchIncDiscover (t:ts) = debug ("Processing log with " ++ show (length (t:ts)) 
                                    ++ " traces")   
                              (batchIncDiscoverR (sort ts) 
                                                 (transform $ tracePPTree 1 t))

batchIncDiscoverR :: (Ord a, Eq a, Show a) => Log a -> PPTree a -> PPTree a 
batchIncDiscoverR []     m = m
batchIncDiscoverR [t]    m = incDiscover t m
batchIncDiscoverR (t:ts) m = batchIncDiscoverR ts 
                                            (incDiscoverDebug t (length ts) m) 

incDiscoverDebug :: (Ord a, Eq a, Show a) => Trace a -> Int 
                                                -> PPTree a -> PPTree a
incDiscoverDebug t ct m = debug ("Processing " ++ show (weight m) ) 
                                (incDiscover t m)

incDiscover :: (Ord a, Eq a, Show a) => Trace a -> PPTree a -> PPTree a
incDiscover t m = transform $ Node2 Choice (tracePPTree 1 t) m (1+weight m)


-- Petri Net conversion
-- Limited to Petri nets of Strings

union3 :: (Ord a) => Set a -> Set a -> Set a -> Set a
union3 x y z = (x `union` y) `union` z

translate :: PPTree String -> WeightedNet
translate ptree = ptreeWeightedNet ptree (Place "I" "pI") (Place "O" "pO") 1

nextid :: Int -> String
nextid x = "t" ++ show (x+1)

midpId :: String -> Place String -> Place String -> String 
midpId newId pi po = placeId pi ++ newId ++ placeId po 

midp :: Int -> Place String 
midp newId = Place "" ("p" ++ show newId) 

-- ptreeWeightedNet PPTree initialPlace finalPlace idOffset
ptreeWeightedNet :: PPTree String -> Place String -> Place String -> Int
            -> WeightedNet
ptreeWeightedNet (Node2 Choice x y w) pi po idp =
        let px = ptreeWeightedNet x pi po (idp+1)
            py = ptreeWeightedNet y pi po (wnmaxnodeid px)
        in WeightedNet (wnplaces px `union` wnplaces py ) 
                       (wntransitions px `union` wntransitions py )
                       (wnedges px `union` wnedges py )
                       pi po (wnmaxnodeid py)

ptreeWeightedNet (Node1 FLoop x m w) pi po idp  
    | m <= 1 = ptreeWeightedNet x pi po idp
    | m > 1  =
        let midp1 = midp (idp+1)
            px      =   ptreeWeightedNet x pi midp1 ( idp+2 )
            nx      =   ptreeWeightedNet (Node1 FLoop x (m-1) w) midp1 po 
                                                        ( wnmaxnodeid px )
        in WeightedNet (union3 (wnplaces px) (wnplaces nx) 
                               (fromList [midp1,pi,po]) ) 
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

ptreeWeightedNet (Node2 Seq x y w) pi po idp =
        let midp1 = midp (idp+1)
            px = ptreeWeightedNet x pi midp1 (idp +2)
            py = ptreeWeightedNet y midp1 po (wnmaxnodeid px)
        in WeightedNet (wnplaces px `union` wnplaces py)
                       (wntransitions px `union` wntransitions py)
                       (wnedges px `union` wnedges py) 
                   pi po (wnmaxnodeid py)

ptreeWeightedNet (Node2 Conc x y w) pi po idp =
    let trantauin   = WTransition "tau" (nextid idp) w
        trantauout  = WTransition "tau" (nextid (idp+1)) 1 
        midpx = midp (idp+3)
        midpy = midp (idp+4)
        endpx = midp (idp+5)
        endpy = midp (idp+6)
        px = ptreeWeightedNet x midpx endpx (idp+7)
        py = ptreeWeightedNet y midpy endpy (wnmaxnodeid px)
    in WeightedNet (union3 (wnplaces px) (wnplaces py) 
                        (fromList[midpx,midpy,endpx,endpy,pi,po]) ) 
                   (union3 (wntransitions px) (wntransitions py) 
                            (fromList[trantauin,trantauout]) ) 
            (union3 (wnedges px) (wnedges py) 
                (fromList [WToTransition pi trantauin, 
                          WToPlace trantauin midpx,WToPlace trantauin midpy,
                          WToTransition endpx trantauout,
                          WToTransition endpy trantauout,
                          WToPlace trantauout po]) ) 
            pi po (wnmaxnodeid py)

ptreeWeightedNet (Leaf x w) pi po idp =
        let tx = WTransition x (nextid idp) w
        in WeightedNet (fromList[pi,po]) (fromList[tx])
                       (fromList [WToTransition pi tx, WToPlace tx po] )
                       pi po (idp+1)

ptreeWeightedNet (Silent w) pi po idp 
    = ptreeWeightedNet (Leaf tau w) pi po (idp+1)
    

validate :: PPTree a -> Bool
validate (Node2 Seq x y w) 
    = w == weight x && w == weight y && validate x && validate y
validate (Node2 Choice x y w) 
    = w == weight x + weight y && validate x && validate y
validate (Node2 Conc x y w)
    = w == weight x + weight y && validate x && validate y
validate (Node1 op x m w)
    = w == weight x && validate x
validate x = True



data Validation = Validation{valResult::Bool, valMsg:: String} 
    deriving (Show,Eq)
valOk = Validation{valResult=True, valMsg="Ok"}
verboseValidate :: (Show a) => PPTree a -> Validation
verboseValidate (Node2 Seq x y w) 
    | w /= weight x    = Validation{valResult=False ,
                                valMsg="Seq " ++ show w ++ " /= " 
                                    ++ show (weight x) ++ " in " ++ sn }
    | w /= weight y    = Validation{valResult=False ,
                                valMsg="Seq " ++ show w ++ " /= " 
                                   ++ show (weight y) ++ " in " ++ sn }
    | not (validate x) = verboseValidate x
    | not (validate y) = verboseValidate y
    | validate  (Node2 Seq x y w)   = valOk
    where sn =  show (Node2 Seq x y w)
verboseValidate (Node2 Choice x y w) 
    | w /= weight x + weight y  = Validation{valResult=False,
                valMsg="Choice " ++ show w ++ " /= " 
                ++ show (weight x) ++ " + " ++ show (weight y) ++ " in " ++ sn}
    | not(validate x) = verboseValidate x 
    | not(validate y) = verboseValidate y
    | validate (Node2 Choice x y w) = valOk
    where sn =  show (Node2 Choice x y w)
verboseValidate (Node2 Conc x y w)
    | w /= weight x + weight y = Validation{valResult=False,
                valMsg="Conc " ++ show w ++ " /= " 
                ++ show (weight x) ++ " + " ++ show (weight y) ++ " in " ++ sn}
    | not(validate x) = verboseValidate x 
    | not(validate y) = verboseValidate y
    | validate (Node2 Conc x y w) = valOk
    where sn =  show (Node2 Conc x y w)
verboseValidate (Node1 op x m w)
    | w /= weight x = Validation{valResult=False,
                valMsg="Node1 " ++ show op ++ " " ++ show w ++ " /= " 
                ++ show (weight x)  }
    | not(validate x) = verboseValidate x
    where sn = show (Node1 op x m w)
verboseValidate x  = valOk


-- main
inputMain = do
    contents <- getContents
    putStr (formatPPTree $ discover parseDelimitedTrace contents)


