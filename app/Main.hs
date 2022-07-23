module Main where

import Data.Maybe (fromMaybe)
import Lib (compile)
import Options.Applicative
import System.IO (hPutStrLn, stderr)

data Paths = Paths {input :: FilePath, output :: Maybe FilePath}

paths :: Parser Paths
paths =
    Paths
        <$> argument str (metavar "input")
        <*> optional (option str (long "output" <> short 'o'))

main :: IO ()
main = do
    Paths{input, output} <- execParser options
    let asmOutput = fromMaybe (toAsmOutput input) output
    src <- readFile input
    case compile src of
        Left err -> hPutStrLn stderr ("Error: " ++ err)
        Right asm -> writeFile asmOutput asm
  where
    options = info (paths <**> helper) idm
    toAsmOutput input = reverse $ ("s" ++) $ dropWhile ('.' /=) $ reverse input
