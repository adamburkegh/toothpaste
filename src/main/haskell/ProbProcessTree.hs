module ProbProcessTree 
    (module ProbProcessTree,
     Weight) where

import PetriNet -- mainly for Weight
import Data.List (nub,sort)


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

-- Loop similarity
(=&=) :: (Eq a) => PPTree a -> PPTree a -> Bool
(Leaf a w1) =&= (Node1 PLoop (Leaf b w2) r w3)
    = a == b
(Node1 PLoop (Leaf b w2) r w3) =&= (Leaf a w1)
    = a == b
(Silent w1) =&= (Node1 PLoop (Silent w2) r w3) 
    = True
(Node1 PLoop (Silent w2) r w3) =&= (Silent w1)
    = True
(Node1 PLoop x r1 w1) =&= (Node1 PLoop y r2 w2) 
    = x =~=  (Node1 PLoop y r2 w2) 
   || y =~=  (Node1 PLoop x r1 w1) 
(NodeN op1 ptl1 w1) =&= (Node1 PLoop y r2 w2) 
    = (NodeN op1 ptl1 w1) =~= y
(Node1 PLoop y r2 w2) =&= (NodeN op1 ptl1 w1) 
    = (NodeN op1 ptl1 w1) =~= y
x =&= y = False

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
pptreeSort poper ptl = pptree poper (sort ptl) 

seqP :: [PPTree a] -> Weight -> PPTree a
seqP = pptree Seq 

choiceP :: (Ord a) => [PPTree a] -> Weight -> PPTree a
choiceP = pptreeSort Choice

concP :: (Ord a) => [PPTree a] -> Weight -> PPTree a
concP (pt:ptl) w = NodeN Conc (sort (pt:ptl)) w
concP [] w     = emptyTree


-- Careful using this one - it breaks consistency and can produce invalid trees
-- Currently only used in converting a loop to a PetriNet - not from rules
replaceWeight :: PPTree a -> Weight -> PPTree a
replaceWeight (Leaf x n) w = Leaf x w
replaceWeight (Silent n) w = Silent w
replaceWeight (Node1 op x r n) w = Node1 op x r w
replaceWeight (NodeN op ptl n) w = NodeN op ptl w


-- tau = "\120591"
tau = "tau" -- encoding issues for Haskell on windows, TODO
latexTau = "\\tau"

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

-- pre: input trees are loop similar
lmerge :: PPTree a -> PPTree a -> PPTree a
lmerge (Node1 PLoop pt2 r w2) pt1
    = merge (Node1 PLoop pt1 1 w1) (Node1 PLoop pt2 r w2)
    where w1 = weight pt1
lmerge pt1 (Node1 PLoop pt2 r w2) 
    = lmerge (Node1 PLoop pt2 r w2) pt1

scale :: PPTree a -> Float -> PPTree a
scale (Leaf x w) g = Leaf x (w*g)
scale (Silent w) g = Silent (w*g)
scale (Node1 op x r w) g = Node1 op (scale x g) r (w*g)
scale (NodeN op ptl w) g = NodeN op (map (`scale` g) ptl) (w*g)

seqMerge :: PPTree a -> PPTree a -> PPTree a
seqMerge x y = scale (merge x y) 0.5

-- Normal form rules promoted to functions
-- Lossy version - includes concurrency flattening 
norm ::  (Ord a, Eq a) => PPTree a -> PPTree a
norm pt | pt == ptn = pt
        | otherwise = ptn
    where ptn = norm1 pt

norm1 :: (Ord a, Eq a) => PPTree a -> PPTree a
norm1 (Leaf x w) = Leaf x w
norm1 (Silent w) = Silent w
norm1 (Node1 FLoop pt r w) 
    | r == 1    = norm pt     -- Fixed Loop id rule
    | otherwise = Node1 FLoop (norm pt) r w
norm1 (Node1 PLoop pt r w) = Node1 PLoop (norm pt) r w
norm1 (NodeN Choice ptl w) = 
        choiceP (flattenList Choice (map norm ptl) ) w
norm1 (NodeN Conc ptl w) = 
        concP (flattenList Conc (map norm ptl)) w
norm1 (NodeN Seq ptl w) = flatten $ NodeN Seq (map norm ptl) w


-- Note included conc flattening
flatten :: (Eq a) => PPTree a -> PPTree a
flatten (NodeN op1 ptl w) = NodeN op1 (flattenList op1 ptl) w
flatten x = x

flattenList :: (Eq a) => POperN -> [PPTree a] -> [PPTree a]
flattenList op1 ((NodeN op2 ptl2 w2):ptl1)
    | op1 == op2 = ptl2 ++ flattenList op1 ptl1
    | otherwise  = NodeN op2 ptl2 w2:ptl1
flattenList op1 (pt:ptl) = pt:flattenList op1 ptl
flattenList op1 [] = []


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

-- LaTeX
--
-- % imports required:
-- \usepackage{tikz}
-- \usetikzlibrary{svg.path,positioning,shapes,calc,arrows.meta} % (maybe)
-- \usepackage{forest}
-- % definitions
-- \def\choicep{\times}
-- \def\concp{\land}
-- \def\seqop{\rightarrow}
-- \def\loopp[#1]{\circlearrowright_p^{#1}}
-- \def\loopn[#1]{\circlearrowright_n^{#1}}
--
latexPPTree :: PPTree String -> String
latexPPTree x = latexFigBegin 
             ++ latexForestBegin 
             ++ (latexPPTreeIndent x 4)
             ++ latexForestEnd
             ++ latexFigEnd

latexFigBegin = "\\begin{figure} \n"
latexFigEnd = 
    "    \\caption{\\LaTeX PPT formatter generated caption} \n\
    \    \\label{fig:formatTest} \n\
    \\\end{figure} \n"

latexForestBegin = 
    "    \\begin{forest} \n\
    \    for tree={edge = {->},math content,anchor=center,fit=tight},\n"
latexForestEnd = "    \\end{forest} \n"

isInt x = x == fromInteger (round x)

latexFloat :: Float -> String
latexFloat n | isInt n   = show (round n)
             | otherwise = show n

latexWeight :: Weight -> String
latexWeight n  = "\\pcol " ++ (latexFloat n)

latexCloseNode = "]\n"

latexLoopOp :: POper1 -> Repeat -> String
latexLoopOp FLoop r = "{\\loopn[" ++ (latexFloat r) ++ "]}"
latexLoopOp PLoop r = "{\\loopp[" ++ (latexFloat r) ++ "]}"

latexNOp :: POperN -> String
latexNOp Choice = "\\choicep"
latexNOp Conc = "\\concp"
latexNOp Seq = "\\seqop"

latexPPTreeIndent :: PPTree String -> Int -> String
latexPPTreeIndent (Leaf x n) indent =
    duplicate indentStr indent ++ "[ \\text{" ++ x ++ "}" ++ latexWeight n 
                               ++ latexCloseNode
latexPPTreeIndent (Silent n) indent =
    duplicate indentStr indent ++ "[ " ++ latexTau ++ latexWeight n 
                               ++ latexCloseNode
latexPPTreeIndent (Node1 op x r n) indent =
    duplicate indentStr indent ++ "[ " ++ (latexLoopOp op r) 
        ++ latexWeight n ++ "\n"
        ++ latexPPTreeIndent x (indent+1)
        ++ duplicate indentStr indent ++ latexCloseNode 
latexPPTreeIndent (NodeN op ptl n) indent =
    duplicate indentStr indent ++ "[ " ++ latexNOp op  
        ++ latexWeight n ++ "\n" 
        ++ concatMap (\pt -> latexPPTreeIndent pt (indent+1) ) ptl  
        ++ duplicate indentStr indent ++ latexCloseNode 

latexRuleBegin =  
    " \\begin{adjustbox}{valign=m} \n\
    \    \\begin{forest} \n\
    \    for tree={edge = {->},math content,anchor=center,fit=tight},\n"
latexRuleEnd = 
    "        \\end{forest} \n \
    \    \\end{adjustbox} \n"
latexOpSubFig = 
    "   \\begin{adjustbox}{valign=t}       \n \
    \       \\raisebox{20pt}{\\huge{$\\slossrule$} }\n \
    \   \\end{adjustbox}  \n"

latexMinipageBegin = "\\begin{minipage}{0.7\\linewidth}"
latexMinipageEnd = "\\end{minipage}\n"

latexRuleEg :: PPTree String -> PPTree String -> String
latexRuleEg pt1 pt2 = latexMinipageBegin 
                   ++ latexRuleBegin 
                   ++ (latexPPTreeIndent pt1 4)
                   ++ latexRuleEnd 
                   ++ latexOpSubFig
                   ++ latexRuleBegin 
                   ++ (latexPPTreeIndent pt2 4)
                   ++ latexRuleEnd 
                   ++ latexMinipageEnd
                   ++ (latexValidStatus pt1)
                   ++ (latexValidStatus pt2)

latexValidStatus :: PPTree String -> String
latexValidStatus pt | validate pt = ""
                    | otherwise  = "\nWARNING: Invalid tree\n"

-- not really duplicating if you do it n times is it
duplicate :: [a] -> Int -> [a]
duplicate sq n = concat $ replicate n sq


ncount :: PPTree a -> Int
ncount (Leaf a _) = 1
ncount (Silent _) = 1
ncount (Node1 op a _ _) = 1 + ncount a
ncount (NodeN op ptl _)  = 1 + foldl (\c pt -> c + ncount pt) 0 ptl


-- validation
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
                                  ++ " ... "
                                  ++ valMsg (verboseValidateList ptl) }
verboseValidateList []  = valOk


