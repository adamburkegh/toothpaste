module Toothpaste.Flowpaste where
-- Flowpaste is a copy/paste/edit fork of the stochastic version, Toothpaste,
-- rather than a beautiful sharing of abstractions, at this point in time

import Toothpaste.PetriNet
import Toothpaste.EventLog
import Toothpaste.TraceUtil
import Data.List (sort)
import Data.Set (Set,fromList,union)

-- Some data structures and manipulations for process trees and 
-- similar structures

changeDebug :: Bool -> String -> a -> a
changeDebug cond msg x = if cond then debug ("=> " ++ msg) x else x

-- Process Trees

data PTree a = Leaf a | Silent
               | Node1 POper1 (PTree a)
               | Node2 POper2 (PTree a) (PTree a) 
                deriving (Show,Ord)

data POper2 = Choice | Seq | Conc deriving (Enum,Show,Eq,Ord)
data POper1 = Loop deriving (Enum,Show,Eq,Ord)
data POper = POper1 | POper2

instance (Eq a) => Eq (PTree a) where
    Silent == Silent = True
    Leaf a == Leaf b = a == b
    Node1 p1 a == Node1 p2 b = (p1 == p2) && (a == b)
    Node2 po1 pt1a pt1b == Node2 po2 pt2a pt2b = (po1 == po2) 
                                            && (pt1a == pt2a)
                                            && (pt1b == pt2b)
    x == y = False

-- partial 
choice :: (Ord a) => [PTree a] -> PTree a
choice [x] = x
choice (x:xs) | x <= head xs = Node2 Choice x (choice xs)
              | otherwise    = Node2 Choice (head xs) (choice (x: tail xs) )

conc :: (Ord a) => [PTree a] -> PTree a
conc [x] = x
conc (x:xs) | x <= head xs = concPair x (conc xs)
            | otherwise    = concPair (head xs) (conc (x: tail xs) )

concPair :: (Ord a) => PTree a -> PTree a -> PTree a
concPair x y | x <= y    = Node2 Conc x y 
             | otherwise = Node2 Conc y x 

seqNode :: [PTree a] -> PTree a
seqNode [] = Silent
seqNode [x] = x
seqNode (x:xs) = Node2 Seq x (seqNode xs)


formatPTree :: (Show a) => PTree a -> String
formatPTree x = formatPTreeIndent x 0

indentStr = "  "

formatPTreeIndent :: (Show a) => PTree a -> Int -> String
formatPTreeIndent (Leaf x) indent = 
    duplicate indentStr indent ++ show x ++ "\n"
formatPTreeIndent (Node1 op x) indent = 
    duplicate indentStr indent ++ show op ++ "\n" 
        ++ formatPTreeIndent x (indent+1)
formatPTreeIndent (Node2 op x y) indent = 
    duplicate indentStr indent ++ show op ++ "\n"
        ++ formatPTreeIndent x (indent+1)
        ++ formatPTreeIndent y (indent+1)
formatPTreeIndent Silent indent = 
    duplicate indentStr indent ++ tau ++ "\n"

duplicate string n = concat $ replicate n string

putPTree :: (Show a) => PTree a -> IO ()
putPTree = putStrLn . formatPTree 


data TRule a = TRule{ rulename :: String, trule :: PTree a -> PTree a}

mapRuleList :: (Show a, Eq a) => PTree a -> [TRule a] -> PTree a 
mapRuleList x [] = x
mapRuleList x [r] =  changeDebug (x /= y) (rulename r ++ " " ++ show x 
                                                      ++ " => " ++ show y) 
                     y where y = trule r x
mapRuleList x (r:rs) = changeDebug (y /= z) (rulename r ++ " " ++ show z
                                                        ++ " => " ++ show y) 
                        y where y = trule r z
                                z = mapRuleList x rs

shrunk :: (Eq a) => PTree a -> PTree a -> Bool
shrunk pt1 pt2 = pt1 /= pt2 && ncount pt1 < ncount pt2

-- This is the ugliest Haskell code ever written
--
-- It takes an input rule, and returns a commutative version of that rule
-- which is distributed over the operator parameter
-- Eg choice is commutative, so choiceId is passed to this rule as
--    commutChoice = commutRule Choice choiceIdT
-- This lets binary trees be treated as multi-child trees and so Haskell
-- pattern matching rules only need to be written as two parameter rules,
-- mostly
commutRule :: (Eq a) => POper2 -> TRule a -> PTree a -> PTree a
commutRule op1 rule (Node2 op2 x (Node2 op3 y z)) 
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
      where inp  = Node2 op2 x (Node2 op3 y z)
            inp2 = Node2 op2 (Node2 op2 x y) z
            inp3 = Node2 op2 (Node2 op2 x z) y   
            rsp  = trule rule inp
            rsp2 = trule rule inp2
            rsp3 = trule rule inp3
            rsr  = Node2 op2 x (commutRule op1 rule (Node2 op3 y z) )
            rsr2 = Node2 op2 (commutRule op1 rule (Node2 op2 x y)) z
            rsr3 = Node2 op2 (commutRule op1 rule (Node2 op2 x z)) y
commutRule op1 rule (Node2 op2 (Node2 op3 x y) z) 
    | op1 == op2 && op1 == op3 && rs /= inp = rs
      where inp = Node2 op1 x (Node2 op1 y z)
            rs  = commutRule op1 rule inp
commutRule op rule x = trule rule x

commutTRule :: (Eq a) => POper2 -> (PTree a -> PTree a) -> String -> TRule a
commutTRule oper rule rname = TRule{rulename="commute " ++ rname, 
                            trule=commutRule oper TRule{rulename="rname", 
                                                        trule=rule }}


-- Takes an input rule, and returns an associative version of that rule
-- which is distributed over the operator parameter
-- Eg sequence is associative and choiceFold may need to use that
-- See commutRule
-- assocRule :: (Eq a) => POper2 -> TRule a -> PTree a -> PTree a
-- assocRule op1 rule (Node2 op2 x (Node2 op3 y z)) 
--     | op1 == op2 && op1 == op3 && rsp /= inp                 = rsp
--     | op1 == op2 && op1 == op3 && rsp == inp && rsp2 /= inp2 = rsp2
--       where inp  = Node2 op2 x (Node2 op3 y z)
--             inp2 = Node2 op2 (Node2 op2 x y) z
--             rsp  = trule rule inp
--             rsp2 = trule rule inp2
--             rsr  = Node2 op2 x (assocRule op1 rule (Node2 op3 y z) )
--             rsr2 = Node2 op2 (assocRule op1 rule (Node2 op2 x y)) z
-- assocRule op1 rule (Node2 op2 (Node2 op3 x y) z) 
--     | op1 == op2 && op1 == op3 = 
--             assocRule op1 rule (Node2 op1 x (Node2 op1 y z))
-- assocRule op rule x = trule rule x



choiceId :: (Eq a) => PTree a -> PTree a
choiceId (Node2 Choice x y) 
             | x == y = x
choiceId x = x 

choiceFold :: (Eq a, Ord a) => PTree a -> PTree a
choiceFold (Node2 Choice 
                (Node2 Seq x1 y1) 
                (Node2 Seq x2 y2) ) 
                | x1 == x2 = Node2 Seq x1 (Node2 Choice y1 y2) 
                | y1 == y2 = Node2 Seq (Node2 Choice x1 x2) y1
                | z1 == z2 = Node2 Seq (Node2 Choice h1 h2) z1
                    where (Node2 Seq h1 z1) = seqSuffix (Node2 Seq x1 y1) 
                          (Node2 Seq h2 z2) = seqSuffix (Node2 Seq x2 y2)
choiceFold (Node2 Choice (Node2 Seq x1 y1) 
                         (Node2 Seq (Node1 Loop x2) y2) ) 
                | x1 == x2 = Node2 Seq (Node1 Loop x1)
                                       (Node2 Choice y1 y2) 
choiceFold (Node2 Choice 
                (Node2 Seq (Node1 Loop x1) y1) 
                (Node2 Seq x2 y2) ) 
                | x1 == x2 = Node2 Seq (Node1 Loop x1)
                                       (Node2 Choice y1 y2) 
choiceFold x = x

choiceSkip :: Eq a => PTree a -> PTree a
choiceSkip (Node2 Choice x
                        (Node2 Seq y z) )  
                  | x == y    = Node2 Seq x (Node2 Choice z Silent)
                  | x == z    = Node2 Seq (Node2 Choice y Silent) x
                  | x == z1   = Node2 Seq (Node2 Choice h1 Silent) x
                     where (Node2 Seq h1 z1) = seqSuffix (Node2 Seq y z) 
choiceSkip (Node2 Choice (Node2 Seq x y) z) 
                  | x == z    = Node2 Seq x (Node2 Choice y Silent)
                  | y == z    = Node2 Seq (Node2 Choice x Silent) y
                  | y1 == z   = Node2 Seq (Node2 Choice h1 Silent) z
                     where (Node2 Seq h1 y1) = seqSuffix (Node2 Seq x y) 
choiceSkip x = x

loopRoll :: Eq a => PTree a -> PTree a
loopRoll (Node2 Seq x y) | x == y = Node1 Loop x
loopRoll (Node2 Seq x (Node2 Seq y z) ) | x == y = Node2 Seq (Node1 Loop x) z
loopRoll (Node2 Seq x (Node1 Loop y) ) | x == y = Node1 Loop x
loopRoll (Node2 Seq x (Node2 Seq (Node1 Loop y) z) ) 
                        | x == y = Node2 Seq (Node1 Loop x) z
loopRoll (Node2 Seq (Node1 Loop x) y ) | x == y = Node1 Loop x
loopRoll (Node2 Seq (Node1 Loop x) (Node2 Seq y z) ) 
                        | x == y = Node2 Seq (Node1 Loop x) z
loopRoll x = x

loopNest :: PTree a -> PTree a
loopNest (Node1 Loop (Node1 Loop x)) = Node1 Loop x
loopNest x = x


-- assoc 
normSeq :: (Eq a) => PTree a -> PTree a 
normSeq (Node2 Seq (Node2 Seq x y) z) = 
    Node2 Seq (normSeq x) (normSeq (Node2 Seq y z))
normSeq (Node2 Seq x y)  = Node2 Seq (normSeq x) (normSeq y)
normSeq x = x

longSeq :: (Eq a) => PTree a -> PTree a
longSeq (Node2 Seq x y) = lrs $ normSeq (Node2 Seq x y)
longSeq x = x

-- pre: both normalized
prefix :: (Eq a) => PTree a -> PTree a -> Bool
prefix (Node2 Seq x1 y1) (Node2 Seq x2 y2) 
    | x1 == x2 = prefix y1 y2
    | x1 /= x2 = False
prefix x (Node2 Seq y z) = x == y
prefix pt1 pt2           = pt1 == pt2

-- pre: start is normalized sequence form as per normSeq
-- Isn't actually longest repeated substring
lrs :: (Eq a) => PTree a -> PTree a 
lrs (Node2 Seq x (Node2 Seq y z)) 
    | prefix x y && cuty /= Silent = Node2 Seq (Node1 Loop x) (Node2 Seq cuty z)
    | prefix x y && cuty == Silent = Node2 Seq (Node1 Loop x) z
    | prefix newsub z && newsuf /= Silent  
                = Node2 Seq (Node1 Loop newsub) newsuf
    | prefix newsub z && newsuf == Silent  
                = Node1 Loop newsub
    where newsub  = Node2 Seq x y
          cuty   = rmpref x y
          newsuf = rmpref newsub z
lrs (Node2 Seq x y) 
    | prefix x y && newsuf /= Silent = Node2 Seq (Node1 Loop x) newsuf
    | prefix x y && newsuf == Silent = Node1 Loop x
                        where newsuf = rmpref x y
lrs x = x

-- rmpref (prefix,ptree)
rmpref :: (Eq a) => PTree a -> PTree a -> PTree a 
rmpref (Node2 Seq x1 y1) (Node2 Seq x2 y2) 
    | x1 == x2 && rs /= y2  = rs
    | x1 /= x2              = Node2 Seq x2 y2
    where rs = rmpref y1 y2
rmpref x (Node2 Seq y z) | x == y = rmpref x z
rmpref pt1 pt2           | pt1 == pt2 = Silent
rmpref pr x = x

---

choiceRoll :: Eq a => PTree a -> PTree a
choiceRoll (Node2 Choice x (Node1 Loop y)) 
                  | x == y = Node1 Loop x
choiceRoll (Node2 Choice (Node1 Loop x) (Node2 Seq y z))
                  | x == y = Node2 Seq (Node1 Loop x) (Node2 Choice z Silent) 
    -- seq assoc?
choiceRoll x = x

concId :: Eq a => PTree a -> PTree a
concId (Node2 Conc x y) | x == y = Node1 Loop x 
concId x = x

concFromChoice :: (Eq a, Ord a) => PTree a -> PTree a
concFromChoice (Node2 Choice 
                    (Node2 Seq x1 y1) 
                    (Node2 Seq y2 x2) ) 
               | x1 == x2 && y1 == y2 = concPair x1 y1
concFromChoice (Node2 Choice 
                    (Node2 Seq x1 (Node2 Seq y1 z1)) 
                    (Node2 Seq x2 (Node2 Seq y2 z2))) 
               | x1 == y2 && x2 == y1 = Node2 Seq (concPair x1 y1)
                                                  (Node2 Choice z1 z2)
concFromChoice (Node2 Choice 
                    (Node2 Seq (Node2 Seq x1 y1) z1) 
                    (Node2 Seq (Node2 Seq y2 x2) z2)) 
               | x1 == x2 && y1 == y2 = Node2 Seq (concPair x1 y1) 
                                                  (Node2 Choice z1 z2)
concFromChoice x = x

concSubsume :: (Eq a) => PTree a -> PTree a
concSubsume (Node2 Choice (Node2 Seq x1 y1) 
                          (Node2 Conc x2 y2))
    | x1 == x2 && y1 == y2 = Node2 Conc x1 y1
    | x1 == y2 && y1 == x2 = Node2 Conc x1 y1
concSubsume (Node2 Choice (Node2 Conc x1 y1) 
                          (Node2 Seq x2 y2))
    | x1 == x2 && y1 == y2 = Node2 Conc x1 y1
    | x1 == y2 && y1 == x2 = Node2 Conc x1 y1
concSubsume (Node2 Choice (Node2 Seq (Node2 Conc x1 y1) z1) 
                          (Node2 Seq (Node2 Seq x2 y2) z2) )
    | x1 == x2 && y1 == y2 = Node2 Seq (Node2 Conc x1 y1) (Node2 Choice z1 z2)
    | x1 == y2 && y1 == x2 = Node2 Seq (Node2 Conc x1 y1) (Node2 Choice z1 z2)
    -- obsolete with assoc?
concSubsume (Node2 Choice (Node2 Seq (Node2 Conc x1 y1) z1) 
                          (Node2 Seq x2 (Node2 Seq y2 z2) ) )
    | x1 == x2 && y1 == y2 = Node2 Seq (Node2 Conc x1 y1) (Node2 Choice z1 z2)
    | x1 == y2 && y1 == x2 = Node2 Seq (Node2 Conc x1 y1) (Node2 Choice z1 z2)
    -- obsolete with assoc?
concSubsume x = x

silentSeq (Node2 Seq x Silent) = x 
silentSeq (Node2 Seq Silent x) = x 
silentSeq x = x

silentConc (Node2 Conc x Silent) = x 
silentConc (Node2 Conc Silent x) = x 
silentConc x = x


seqSuffix1 :: [PTree a] -> PTree a -> PTree a
seqSuffix1 head (Node2 Seq x y) = seqSuffix1 (head++[x]) y
seqSuffix1 head x = Node2 Seq (seqNode head) x

seqSuffix :: (Eq a) => PTree a -> PTree a
seqSuffix (Node2 Seq x y) = seqSuffix1 [] (Node2 Seq x y)
seqSuffix x = x


-- Associativity rules
seqAssoc :: Eq a => PTree a -> PTree a
seqAssoc (Node2 Seq x (Node2 Seq y z) ) = Node2 Seq (Node2 Seq x y) z
seqAssoc x = x

-- choice and conc rules enforce lexical order
choiceAssoc :: (Eq a, Ord a) => PTree a -> PTree a
choiceAssoc (Node2 Choice x (Node2 Choice y z) )  
                = Node2 Choice (Node2 Choice r1 r2) r3 
                where [r1,r2,r3] = sort [x,y,z]
choiceAssoc x = x

concAssoc :: (Eq a, Ord a) => PTree a -> PTree a
concAssoc (Node2 Conc x (Node2 Conc y z) )  
                = Node2 Conc (Node2 Conc r1 r2) r3 
                where [r1,r2,r3] = sort [x,y,z]
concAssoc x = x

opsep = " :: "
nsep  = " --- "


-- Note this is still naive 
-- It has the hill-climbing problem of any productive step getting locked in
-- Need to do something like inductive miner instead where we 
-- do an iteration-limited exploration of different full trees
transformWithRules :: (Show a, Eq a) => PTree a -> [TRule a] -> PTree a
transformWithRules (Node1 op x) rules = 
    mapRuleList( Node1 op 
                    (transformWithRules x rules) )  
             rules
transformWithRules (Node2 op x y) rules = 
    mapRuleList (Node2 op
                   (transformWithRules x rules)  
                  (transformWithRules y rules) )  
            rules
transformWithRules x _ = x

maxTransform :: (Show a, Eq a) => PTree a -> [TRule a] -> PTree a
maxTransform x rules | x == y      = x
                     | otherwise   = maxTransform y rules
                     where y = debug ("=== Count:" ++ show (ncount x) ++  "===") (transformWithRules x rules)

baseRuleList :: (Eq a, Ord a) => [TRule a]
baseRuleList = [commutTRule Choice choiceId "choiceId",
            commutTRule Choice choiceFold "choiceFold",
            commutTRule Choice choiceSkip "choiceSkip",
            commutTRule Choice choiceRoll "choiceRoll",
            commutTRule Conc concId "concId",
            commutTRule Choice concSubsume "concSubsume",
            commutTRule Choice concFromChoice "concFromChoice",
            TRule{rulename="loopRoll",trule=loopRoll},
            TRule{rulename="loopNest",trule=loopNest},
            TRule{rulename="silentSeq",trule=silentSeq},
            commutTRule Conc silentConc "silentConc", 
            TRule{rulename="longSeq",trule=longSeq} ]

traceLoopRules :: (Eq a) => [TRule a]
traceLoopRules = [TRule{rulename="longSeq",trule=longSeq},
                  TRule{rulename="loopRoll",trule=loopRoll} ]

ruleList :: (Show a, Eq a, Ord a) => [TRule a]
ruleList =  baseRuleList

flipRules :: (Eq a, Ord a) => [TRule a]
flipRules = [TRule{rulename="seqAssoc",trule=seqAssoc},
             TRule{rulename="choiceAssoc",trule=choiceAssoc},
             TRule{rulename="concAssoc",trule=concAssoc}]

transform :: (Show a, Eq a, Ord a) => PTree a -> PTree a
transform = transformClean
-- transform = transformWithFlip -- works better, but loops

transformClean x = maxTransform x ruleList 

-- loop issues with flips
transformWithFlip x =   maxTransform 
                    (transformWithRules
                        (maxTransform x ruleList)
                        flipRules)
                    ruleList

ncount :: PTree a -> Int
ncount (Leaf a) = 1
ncount Silent = 1
ncount (Node1 p a ) = 1 + ncount a 
ncount (Node2 p a b)  = 1 + ncount a  + ncount b 


-- mining

terminalLabel = "$$"
terminal = Leaf terminalLabel

traceModel :: Log String -> PTree String
-- traceModel x = Node2 Seq (traceModelR x) terminal -- One terminal
traceModel = traceModelR . sort -- No terminal
-- traceModel = traceModelT -- Terminal per trace
-- traceModel = traceModelLoop -- identify loops at trace assembly

traceModelLoop :: Log String -> PTree String
traceModelLoop x = transformWithRules (traceModelR x) 
                        traceLoopRules

traceModelR :: Log String -> PTree String
traceModelR [] = terminal
traceModelR [x] = tracePTree x
-- trace order version: inferior, but showed a rule bug
-- traceModel (x:xs) = Node2 Choice (tracePTree x) (traceModel xs)
-- lexical order version - better, but concealed a bug 
-- in association rules
traceModelR (x:y:xs)  
           | x == y  = traceModelR (x:xs)
           | x /= y  = Node2 Choice u1 u2
            where u1 = tracePTree x
                  u2 = traceModelR (y:xs)

traceModelT :: Log String -> PTree String
traceModelT [] = terminal
traceModelT [x] = tracePTreeT x
traceModelT (x:xs)
           | u1 <= u2  = Node2 Choice u1 u2
           | otherwise = Node2 Choice u2 u1            
            where u1 = tracePTreeT x 
                  u2 = traceModelR xs

tracePTree :: Trace String -> PTree String
tracePTree [x] = Leaf x
tracePTree (x:xs) = Node2 Seq (Leaf x) (tracePTree xs) 

tracePTreeT :: Trace String -> PTree String
tracePTreeT [x] = Node2 Seq (Leaf x) terminal
tracePTreeT (x:xs) = Node2 Seq (Leaf x) (tracePTreeT xs) 

discover :: Parser -> String -> PTree String
discover parser = transform . traceModel . parser


-- Petri Net conversion
-- Limited to Petri nets of Strings

union3 :: (Ord a) => Set a -> Set a -> Set a -> Set a
union3 x y z = (x `union` y) `union` z

-- tau = "\120591"
tau = "tau" -- encoding issues for Haskell on windows, TODO

translate :: PTree String -> PetriNet String
translate ptree = wfNetToPetriNet $ translateWFNet ptree

translateWFNet :: PTree String -> WorkflowNet
translateWFNet ptree = ptreeWFNet ptree (Place "I" "pI") (Place "O" "pO") 1

wfNetToPetriNet net = (places net, transitions net, edges net)

nextid :: Int -> String
nextid x = "t" ++ show (x+1)

midpId :: String -> Place String -> Place String -> String 
midpId newId pi po = placeId pi ++ newId ++ placeId po 

midp :: Int -> Place String 
midp newId = Place "" ("p" ++ show newId) 

-- ptreeWFNet PTree initialPlace finalPlace idOffset
ptreeWFNet :: PTree String -> Place String -> Place String -> Int
            -> WorkflowNet
ptreeWFNet (Node2 Choice x y) pi po idp =
        let px = ptreeWFNet x pi po (idp+1)
            py = ptreeWFNet y pi po (maxnodeid px)
        in WorkflowNet (places px `union` places py ) 
                       (transitions px `union` transitions py )
                       (edges px `union` edges py )
                       pi po (maxnodeid py)

ptreeWFNet (Node1 Loop x) pi po idp = 
    let midp1 = midp (idp+1)
        trantauin  = SilentTransition  (nextid (idp+2))
        trantauout = SilentTransition  (nextid (idp+3)) 
        px      =   ptreeWFNet x midp1 midp1 ( idp+4 )
    in WorkflowNet (places px `union` fromList [midp1,pi,po] ) 
                   (transitions px `union` fromList [trantauin,trantauout] )
                   (edges px `union` 
                        fromList [ToTransition pi trantauin,
                                   ToPlace trantauin midp1,
                                   ToTransition midp1 trantauout,
                                   ToPlace trantauout po ]  )
                   pi po (maxnodeid px)

ptreeWFNet (Node2 Seq x y) pi po idp 
    | y == terminal  = 
        let px = ptreeWFNet x pi po (idp+1)
        in WorkflowNet (places px)
                       (transitions px)
                       (edges px) 
                        pi po (maxnodeid px+1)
    | otherwise      =
        let midp1 = midp (idp+1)
            px = ptreeWFNet x pi midp1 (idp +2)
            py = ptreeWFNet y midp1 po (maxnodeid px)
        in WorkflowNet (places px `union` places py)
                       (transitions px `union` transitions py)
                       (edges px `union` edges py) 
                   pi po (maxnodeid py)

ptreeWFNet (Node2 Conc x y) pi po idp =
    let trantauin   = SilentTransition (nextid idp)
        trantauout  = SilentTransition (nextid (idp+1) )
        midpx = midp (idp+3)
        midpy = midp (idp+4)
        endpx = midp (idp+5)
        endpy = midp (idp+6)
        px = ptreeWFNet x midpx endpx (idp+7)
        py = ptreeWFNet y midpy endpy (maxnodeid px)
    in WorkflowNet (union3 (places px) (places py) 
                        (fromList[midpx,midpy,endpx,endpy,pi,po]) ) 
                   (union3 (transitions px) (transitions py) 
                            (fromList[trantauin,trantauout]) ) 
            (union3 (edges px) (edges py) 
                (fromList [ToTransition pi trantauin, 
                          ToPlace trantauin midpx,ToPlace trantauin midpy,
                          ToTransition endpx trantauout,
                          ToTransition endpy trantauout,
                          ToPlace trantauout po]) ) 
            pi po (maxnodeid py)

ptreeWFNet (Leaf x) pi po idp
    | x == terminalLabel = ptreeWFNet (Leaf tau) pi po (idp+1)
    | otherwise = 
        let tx = Transition x (nextid idp) 
        in WorkflowNet (fromList[pi,po]) (fromList[tx])
                       (fromList [ToTransition pi tx, ToPlace tx po] )
                       pi po (idp+1)

ptreeWFNet Silent pi po idp = 
        let tx = SilentTransition (nextid idp) 
        in WorkflowNet (fromList[pi,po]) (fromList[tx])
                       (fromList [ToTransition pi tx, ToPlace tx po] )
                       pi po (idp+1)


-- main
inputMain = do
    contents <- getContents
    putStr (formatPTree $ discover parseDelimitedTrace contents)

main = inputMain

