-- Event log parsers for a few formats

module EventLog where

import Data.List.Split
import Data.Map 
    (Map,empty,member,insert,union,size,difference,fromList,toList,lookup)
import Data.Maybe (fromJust)

-- Event logs
type Trace e = [e]
type Log   e = [Trace e]

type Parser = String -> Log String

parseDelimitedTrace :: String -> Log String
parseDelimitedTrace = filter (/= []) . map words . lines

-- Double comma delimited
parseDCDT :: String -> Log String
parseDCDT = map (splitOn ",,") . lines

-- Activity indexing decorator
-- actIndex :: Log String -> (String -> Int, Log Int) 


traceIndex :: (Ord e) => Trace e -> Map e Int
traceIndex t = traceIndexR t empty 0

traceIndexR :: (Ord e) => Trace e -> Map e Int -> Int -> Map e Int
traceIndexR [] m mx = m
traceIndexR (a:ta) m mx
    | member a m        = traceIndexR ta m mx
    | not (member a m)  = traceIndexR ta (insert a (mx+1) m) (mx+1)

intTrace :: (Ord e) => Trace e -> Map e Int -> Trace Int
intTrace [] m     = []
intTrace (x:xs) m = (fromJust (Data.Map.lookup x m) ):(intTrace xs m)

inverseMap :: Map e Int -> Map Int e
inverseMap x = fromList (map (\(x, y) -> (y,x)) (toList x) )

logIndex :: (Ord e) => Log e -> (Log Int, Map Int e)
logIndex lg = (rs, inverseMap m)
    where (rs,m) = logIndexR lg empty 0

logIndexR :: (Ord e) => Log e -> Map e Int -> Int -> (Log Int, Map e Int)
logIndexR [] m _      = ([],m)
logIndexR (t:ts) m mx 
    | size(df)  > 0 = ((intTrace t nm):nl, union rh nm)
    | size(df) == 0 = ((intTrace t m):zl, zm) 
        where rh      = traceIndexR t m mx
              df      = difference rh m
              (nl,nm) = logIndexR ts rh (mx+size(df))
              (zl,zm) = logIndexR ts m mx


