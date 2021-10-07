module TraceUtil where

import Debug.Trace

-- debug and trace
debug :: String -> a -> a
-- Debug ON
debug = trace

-- Debug OFF
-- debug x y = y

warn :: String -> a -> a
warn msg x = trace ("WARNING " ++ msg) x

