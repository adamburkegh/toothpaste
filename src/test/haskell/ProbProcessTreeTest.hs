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
            "        [ a\\colon 1.0]\n"
            ++ forEnd
                    ~=? latexPPTree la,
     "silent" ~: 
            forStart ++ 
            "        [ \\tau\\colon 2.0]\n"
            ++ forEnd
                    ~=? latexPPTree (Silent 2),
     "floop" ~: forStart ++
            "        [ {\\loopn[3.0]}\\colon 1.0\n\
            \          [ a\\colon 1.0]\n\
            \        ]\n"
            ++ forEnd
              ~=? latexPPTree (Node1 FLoop la 3 1),
     "ploop" ~: forStart ++
            "        [ {\\loopp[2.0]}\\colon 1.0\n\
            \          [ b\\colon 1.0]\n\
            \        ]\n"
            ++ forEnd
              ~=? latexPPTree (Node1 PLoop lb 2 1),
      "choice" ~: forStart ++ 
            "        [ \\choicep\\colon 2.0\n\
            \          [ a\\colon 1.0]\n\
            \          [ b\\colon 1.0]\n\
            \        ]\n"
            ++ forEnd
              ~=? latexPPTree (NodeN Choice [la,lb] 2)
                    ]

huTests = latexFormatTests

