import BinpasteTest (huTests)
import FlowpasteTest (huTests)
import EventLogTest (huTests)
import Binpaste -- ghci convenience

import System.Exit
import Test.HUnit

main :: IO ()
main = do
    results <- runTestTT $ 
                    test (BinpasteTest.huTests 
                       ++ FlowpasteTest.huTests
                       ++ EventLogTest.huTests)
    if errors results + failures results == 0 then
        putStrLn "Tests passed."
    else
        die "Tests failed."


