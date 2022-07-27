module Main where

import System.Exit (exitSuccess)
import TestFlParser (testFlParser)
import TestElcTranslator (testElcTranslator)

main :: IO ()
main = do
    testFlParser
    testElcTranslator
    exitSuccess
