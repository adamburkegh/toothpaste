module ProbProcessTreeTest where

import ProbProcessTree

import Test.HUnit

la   = Leaf "a" 1
lb   = Leaf "b" 1
lc   = Leaf "c" 1
ld   = Leaf "c" 1
ld3  = Leaf "d" 3

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
                                3] 3 )
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

huTests = normTests ++ latexFormatTests

