module ProbProcessTreeTest where

import ProbProcessTree

import Test.HUnit

la   = Leaf "a" 1
lb   = Leaf "b" 1
lc   = Leaf "c" 1
ld   = Leaf "c" 1
ld3  = Leaf "d" 3
lfa  = Node1 FLoop la 3 1
lfa3 = Node1 FLoop la 3 3
lfb  = Node1 FLoop lb 3 1
lpa  = Node1 PLoop la 3 1
lpa3 = Node1 PLoop la 3 3
lpb  = Node1 PLoop lb 3 1
sab  = NodeN Seq [la,lb] 1

normTests = [
    "leaf" ~: la ~=? norm la,
    "choice1" ~: choiceP [la,lb,lc] 3 ~=? 
                    norm (NodeN Choice [la,
                                       NodeN Choice [lb,lc] 2] 
                                3),
    "choice2" ~: choiceP [la,Silent 1,lc] 3 ~=? 
                    norm (NodeN Choice [la,
                                       NodeN Choice [Silent 1,lc] 2] 
                                3),
    "seqChoice1" ~: seqP [ld3,choiceP [la,Silent 1,lc] 3] 3 ~=? 
        norm (NodeN Seq [ld3,
                         NodeN Choice [la,
                                       NodeN Choice [Silent 1,lc] 2] 
                                3] 3 ),
    "floop1"  ~: la   ~=? norm (Node1 FLoop la 1 1),
    "floop2"  ~: lfb  ~=? norm lfb,
    "loop"    ~: lpa3 ~=? norm lpa3
    ]

simTests = [ 
    "leaf"      ~: True  ~=? la =~= la ,
    "leafNot"   ~: False ~=? la =~= lb 
    ]


lsimTests = [ 
    "fixedIdentity"  ~: False ~=? lfa =&= lfa ,
    "probIdentity"   ~: False ~=? lpa =&= lpa ,
    "leaf"           ~: True  ~=? lpa =&= la ,
    "leafNot"        ~: False ~=? lpa =&= lb,
    "probSeq"        ~: True ~=? Node1 PLoop sab 5 1 =&= sab,
    "fixedDiffActivities"      ~: False ~=? lfa =&= lfb,
    "fixedDiffWeights"         ~: False ~=? lfa =&= lfa3,
    "probDiffActivities"       ~: False ~=? lpa =&= lpb,
    "probDiffWeights"          ~: False  ~=? lpa =&= lpa3,
    "probDiffLoop"             ~: False ~=? lpa =&= lfa,
    "probLoopNest"             ~: True  ~=? lpa =&= Node1 PLoop lpa3 5 3
    ]



forStart = "\\begin{figure} \n\
            \    \\begin{forest} \n\
            \    for tree={edge = {->},math content,anchor=center,fit=tight},\n"
forEnd =    "    \\end{forest} \n\
            \    \\caption{\\LaTeX PPT formatter generated caption} \n\
            \    \\label{fig:formatTest} \n\
            \\\end{figure} \n"

latexFormatTests = [ 
    "leaf" ~: 
            forStart ++ 
            "        [ \\text{a}\\pcol 1]\n"
            ++ forEnd
                    ~=? latexPPTree la,
    "floatyLeaf" ~: 
            forStart ++ 
            "        [ \\text{a}\\pcol 1.4]\n"
            ++ forEnd
                    ~=? latexPPTree (Leaf "a" 1.4),
     "silent" ~: 
            forStart ++ 
            "        [ \\tau\\pcol 2]\n"
            ++ forEnd
                    ~=? latexPPTree (Silent 2),
     "floop" ~: forStart ++
            "        [ {\\loopn[3]}\\pcol 1\n\
            \          [ \\text{a}\\pcol 1]\n\
            \        ]\n"
            ++ forEnd
              ~=? latexPPTree (Node1 FLoop la 3 1),
     "ploop" ~: forStart ++
            "        [ {\\loopp[2]}\\pcol 1\n\
            \          [ \\text{b}\\pcol 1]\n\
            \        ]\n"
            ++ forEnd
              ~=? latexPPTree (Node1 PLoop lb 2 1),
      "choice" ~: forStart ++ 
            "        [ \\choicep\\pcol 2\n\
            \          [ \\text{a}\\pcol 1]\n\
            \          [ \\text{b}\\pcol 1]\n\
            \        ]\n"
            ++ forEnd
              ~=? latexPPTree (NodeN Choice [la,lb] 2)
                    ]

huTests =   normTests 
         ++ simTests  ++ lsimTests
         ++ latexFormatTests




