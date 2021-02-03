-- Event log parsers for a few formats

module EventLog where

import Data.List.Split

-- Event logs
type Trace e = [e]
type Log   e = [Trace e]

type Parser = String -> Log String

parseDelimitedTrace :: String -> Log String
parseDelimitedTrace = filter (/= []) . map words . lines

-- Double comma delimited
parseDCDT :: String -> Log String
parseDCDT = map (splitOn ",,") . lines

