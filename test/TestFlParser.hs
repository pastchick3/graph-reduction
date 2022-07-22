module TestFlParser where

import FlDef qualified as Fl
import FlParser qualified (parse)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)

testFlParser1 :: IO ()
testFlParser1 = do
    case FlParser.parse "t=1" of
        Right prog | prog == Fl.Prog [Fl.DefVar (Fl.PatVar (Fl.LowerVar "t")) (Fl.ExpInt 1)] -> pure ()
        Right prog -> hPutStrLn stderr ("Error: " ++ show prog) >> exitFailure
        _ -> hPutStrLn stderr "Error: Parser Fail" >> exitFailure
