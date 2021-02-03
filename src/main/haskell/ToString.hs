module ToString where

import Data.Typeable

-- This is a bit hacky, but allows strings to be unquoted, while avoiding
-- extra quotes, pragmas and type explosion
-- ... well except Typeable which ends up leaking everywhere
-- Via https://stackoverflow.com/questions/41095748/how-to-convert-arbitrary-type-to-string-without-adding-extra-quotes-to-strings

toString :: (Show a, Typeable a) => a -> String
toString x = case cast x of
   Just y  -> y
   Nothing -> show x


