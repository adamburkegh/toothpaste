import Toothpaste.BinpasteTest (huTests)
import Toothpaste.FlowpasteTest (huTests)
import Toothpaste.ProbProcessTreeTest (huTests)
import Toothpaste.ToothpasteTest (huTests)
import Toothpaste.EventLogTest (huTests)
import Toothpaste.TPMineTest (huTests)
import Toothpaste.TPConformTest (huTests)
import Toothpaste.OtherRulesTest (huTests)
import Toothpaste.WeightedAutomataTest (huTests)

import System.Exit
import Test.HUnit

main :: IO ()
main = do
    results <- runTestTT $ 
                    test (Toothpaste.BinpasteTest.huTests 
                       ++ Toothpaste.FlowpasteTest.huTests
                       ++ Toothpaste.ProbProcessTreeTest.huTests
                       ++ Toothpaste.ToothpasteTest.huTests
                       ++ Toothpaste.TPMineTest.huTests
                       ++ Toothpaste.EventLogTest.huTests
                       ++ Toothpaste.TPConformTest.huTests
                       ++ Toothpaste.OtherRulesTest.huTests
                       ++ Toothpaste.WeightedAutomataTest.huTests)
    --
    if errors results + failures results == 0 then
        putStrLn "Tests passed."
    else
        die "Tests failed."


