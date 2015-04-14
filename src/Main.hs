{-# LANGUAGE OverloadedStrings #-}
module Main where

import Types
import Imports
import Data.Aeson
import qualified Server

main :: IO ()
main = do
    print $ encode.toJSON $ Step "Mix everything" 1 Nothing
    port <- readInt <$> getEnv "PORT"
    Server.run port
