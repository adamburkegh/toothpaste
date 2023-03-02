module Toothpaste.TraceUtil where

import Control.Logger.Simple
import qualified Data.Text as T

-- debug and trace
debug :: String -> a -> a
debug x = pureDebug $ T.pack x

warn :: String -> a -> a
warn x = pureWarn $ T.pack x

