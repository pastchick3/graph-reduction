module Main where


-- import FlParser (parse)
import System.Exit (exitFailure, exitSuccess)

main :: IO ()
main = do
  case True of
    False -> exitSuccess
    _ -> exitFailure