module Main where

import Data.Maybe (fromMaybe)
import Lib (compile)
import Options.Applicative

data Paths = Paths {input :: FilePath, output :: Maybe FilePath}

paths :: Parser Paths
paths =
    Paths
        <$> argument str idm
        <*> optional (option str (long "output" <> short 'o'))

main :: IO ()
main = do
    Paths{input, output} <- execParser opts
    let asmOutput = fromMaybe (toAsmOutput input) output
    src <- readFile input
    let asm = compile src
    writeFile asmOutput asm
  where
    opts = info (paths <**> helper) idm
    toAsmOutput input = reverse $ ("s" ++) $ dropWhile ('.' /=) $ reverse input
