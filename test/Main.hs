module Main where

import FlDef qualified as Fl
import FlParser qualified (parse)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import TestFlParser

main :: IO ()
main = do
  testFlParser1
  exitSuccess
