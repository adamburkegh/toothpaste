import ToothpasteTest (huTests)
import FlowpasteTest (huTests)
import Toothpaste -- ghci convenience

import System.Exit
import Test.HUnit

main :: IO ()
main = do
    results <- runTestTT $ 
                    test (ToothpasteTest.huTests ++ FlowpasteTest.huTests)
    if errors results + failures results == 0 then
        putStrLn "Tests passed."
    else
        die "Tests failed."


