module Main where

import System.Exit (exitSuccess)
import TestFlParser (testFlParser)

main :: IO ()
main = do
    testFlParser
    exitSuccess
