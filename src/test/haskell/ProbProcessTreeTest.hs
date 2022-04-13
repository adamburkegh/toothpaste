module ProbProcessTreeTest where

import ProbProcessTree

import Test.HUnit

la  = Leaf "a" 1
lb  = Leaf "b" 1

forStart = "\\begin{figure} \n\
            \    \\begin{forest} \n\
            \    for tree={edge = {->},math content,anchor=center,fit=tight},\n"
forEnd =    "    \\end{forest} \n\
            \    \\caption{\\LaTeX PPT formatter test} \n\
            \    \\label{fig:formatTest} \n\
            \\\end{figure} \n"

latexFormatTests = [ 
    "leaf" ~: 
            forStart ++ 
            "        [ a\\pcol 1]\n"
            ++ forEnd
                    ~=? latexPPTree la,
    "floatyLeaf" ~: 
            forStart ++ 
            "        [ a\\pcol 1.4]\n"
            ++ forEnd
                    ~=? latexPPTree (Leaf "a" 1.4),
     "silent" ~: 
            forStart ++ 
            "        [ \\tau\\pcol 2]\n"
            ++ forEnd
                    ~=? latexPPTree (Silent 2),
     "floop" ~: forStart ++
            "        [ {\\loopn[3]}\\pcol 1\n\
            \          [ a\\pcol 1]\n\
            \        ]\n"
            ++ forEnd
              ~=? latexPPTree (Node1 FLoop la 3 1),
     "ploop" ~: forStart ++
            "        [ {\\loopp[2]}\\pcol 1\n\
            \          [ b\\pcol 1]\n\
            \        ]\n"
            ++ forEnd
              ~=? latexPPTree (Node1 PLoop lb 2 1),
      "choice" ~: forStart ++ 
            "        [ \\choicep\\pcol 2\n\
            \          [ a\\pcol 1]\n\
            \          [ b\\pcol 1]\n\
            \        ]\n"
            ++ forEnd
              ~=? latexPPTree (NodeN Choice [la,lb] 2)
                    ]

huTests = latexFormatTests

