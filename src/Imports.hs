module Imports (module X,module Imports) where

import Control.Applicative as X ((<$>), (<*>))
import System.Environment as X(getEnv)

readInt :: String -> Int
readInt = read :: String -> Int