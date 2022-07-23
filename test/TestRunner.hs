module TestRunner (runTest) where

import System.Exit (exitFailure)
import System.IO (stderr)
import Text.Printf (hPrintf)

runTest :: (Eq b, Show b) => String -> (a -> Either String b) -> a -> b -> IO ()
runTest file func input expect = case func input of
    Right output | output == expect -> pure ()
    Right output -> report mismatchMsg (show expect) (show output) >> exitFailure
    Left err -> report errMsg err >> exitFailure
  where
    report msg = hPrintf stderr msg file
    mismatchMsg = "Mismatch in `%v`:\nExpect: %v\nOutput: %v"
    errMsg = "Error in `%v`: %v"
