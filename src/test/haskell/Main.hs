import BinpasteTest (huTests)
import FlowpasteTest (huTests)
import ProbProcessTreeTest (huTests)
import ToothpasteTest (huTests)
import EventLogTest (huTests)
import TPMineTest (huTests)
import TPConformTest (huTests)
import OtherRulesTest (huTests)

import System.Exit
import Test.HUnit

main :: IO ()
main = do
    results <- runTestTT $ 
                    test (BinpasteTest.huTests 
                       ++ FlowpasteTest.huTests
                       ++ ProbProcessTreeTest.huTests
                       ++ ToothpasteTest.huTests
                       ++ TPMineTest.huTests
                       ++ EventLogTest.huTests
                       ++ TPConformTest.huTests
                       ++ OtherRulesTest.huTests)
    if errors results + failures results == 0 then
        putStrLn "Tests passed."
    else
        die "Tests failed."


